module GUI.SummaryView (
    SummaryView,
    summaryViewNew,
    summaryViewSetEvents,
    summaryViewSetInterval,
  ) where

import GHC.RTS.Events

import GUI.Timeline.Render.Constants
import GUI.Types

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Data.Array
import Data.IORef
import Data.Maybe
import Data.Word (Word64)
import qualified Data.List as L
import qualified Data.IntMap as IM
import Control.Exception (assert)
import Text.Printf

------------------------------------------------------------------------------

data SummaryView = SummaryView
  { gtkLayout      :: !Layout
  , defaultInfoRef :: !(IORef String)  -- ^ info for interval Nothing, speedup
  , meventsRef     :: !(IORef (Maybe (Array Int CapEvent)))
  , mintervalIORef :: !(IORef (Maybe Interval))
  }

------------------------------------------------------------------------------

summaryViewNew :: Builder -> IO SummaryView
summaryViewNew builder = do
  defaultInfoRef <- newIORef ""
  meventsRef <- newIORef Nothing
  mintervalIORef <- newIORef Nothing
  let getWidget cast = builderGetObject builder cast
  gtkLayout  <- getWidget castToLayout "eventsLayoutSummary"
  let infoView = SummaryView{..}
  -- Drawing
  on gtkLayout exposeEvent $ liftIO $ do
    defaultInfo <- readIORef defaultInfoRef
    mevents <- readIORef meventsRef
    minterval <- readIORef mintervalIORef
    drawSummary gtkLayout defaultInfo mevents minterval
    return True
  return infoView

------------------------------------------------------------------------------

drawSummary :: Layout -> String -> Maybe (Array Int CapEvent)
            -> Maybe Interval -> IO ()
drawSummary gtkLayout defaultInfo mevents minterval = do
  let info = case minterval of
        Nothing -> defaultInfo  -- speedup
        _       -> fst (summaryViewProcessEvents minterval mevents)
  win <- layoutGetDrawWindow gtkLayout
  pangoCtx <- widgetGetPangoContext gtkLayout
  layout <- layoutText pangoCtx info
  layoutSetAttributes layout [AttrFamily minBound maxBound "monospace"]
  (_, Rectangle _ _ width height) <- layoutGetPixelExtents layout
  layoutSetSize gtkLayout (width + 30) (height + 10)
  renderWithDrawable win $ do
    moveTo (fromIntegral ox / 2) (fromIntegral ox / 3)
    showLayout layout

------------------------------------------------------------------------------

summaryViewSetInterval :: SummaryView -> Maybe Interval -> IO ()
summaryViewSetInterval SummaryView{gtkLayout, mintervalIORef} minterval = do
  writeIORef mintervalIORef minterval
  widgetQueueDraw gtkLayout

------------------------------------------------------------------------------

genericSetEvents :: (Maybe (Array Int CapEvent)
                     -> (String, Maybe (Array Int CapEvent)))
                 -> SummaryView -> Maybe (Array Int CapEvent) -> IO ()
genericSetEvents processEvents SummaryView{..} mev = do
  let (defaultInfo, mevents) = processEvents mev
  writeIORef defaultInfoRef defaultInfo
  writeIORef meventsRef mevents
  writeIORef mintervalIORef Nothing  -- the old interval may make no sense
  widgetQueueDraw gtkLayout

-- | Data collected and computed gradually while events are scanned.
data SummaryData = SummaryData
  { dallocTable     :: !(IM.IntMap (Word64, Word64))  -- indexed by caps
  , dcopied         :: Maybe Word64
  , dmaxResidency   :: Maybe Word64
  , dmaxSlop        :: Maybe Word64
  , dmaxMemory      :: Maybe Word64
  , dmaxFrag        :: Maybe Word64
  , dGCTable        :: !(IM.IntMap RtsGC)  -- indexed by caps
  -- Here we store the official +RTS -s timings of GCs, 
  -- that is times aggregated from the main caps of all GCs.
  -- For now only gcElapsed and gcMaxPause are needed, so the rest
  -- of the fields stays at default values.
  , dGCMain         :: Maybe RtsGC
  , dparMaxCopied   :: Maybe Word64
  , dparTotCopied   :: Maybe Word64
  , dmaxParNThreads :: Maybe Int
--, dtaskTable      -- of questionable usefulness, hard to get
  , dsparkTable     :: !(IM.IntMap (RtsSpark, RtsSpark))  -- indexed by caps
--, dInitExitT      -- TODO. At least init time can be included in the total
                    -- time registered in the eventlog. Can we measure this
                    -- as the time between some initial events?
--, dGCTime         -- Is better computed after all events are scanned,
                    -- e.g., because the same info can be used to calculate
                    -- per-cap GCTime and other per-cap stats.
--, dtotalTime      -- TODO: can we measure this excludint INIT or EXIT times?
  }

data RtsSpark = RtsSpark
 { sparkCreated, sparkDud, sparkOverflowed
 , sparkConverted, sparkFizzled, sparkGCd :: !Timestamp
 }

type Gen = Int

type Cap = Int

data GcMode =
  ModeInit | ModeStart | ModeSync Cap | ModeGHC Cap Gen | ModeEnd | ModeIdle
  deriving Eq

data RtsGC = RtsGC
  { gcMode      :: !GcMode
  , gcStartTime :: !Timestamp
  , gcGenStat   :: !(IM.IntMap GenStat)  -- indexed by generations
  }

-- Index at the @gcGenStat@ map at which we store the sum of stats over all
-- generations, or the single set of stats for non-genenerational GC models.
gcGenTot :: Gen
gcGenTot = -1

data GenStat = GenStat
  { -- Sum over all seqential and pararell GC invocations.
    gcAll      :: !Int
  , -- Only parallel GCs. For GC models without stop-the-world par, always 0.
    gcPar      :: !Int
  , gcElapsed  :: !Timestamp
  , gcMaxPause :: !Timestamp
  }

emptySummaryData :: SummaryData
emptySummaryData = SummaryData
  { dallocTable    = IM.empty
  , dcopied        = Nothing
  , dmaxResidency  = Nothing
  , dmaxSlop       = Nothing
  , dmaxMemory     = Nothing
  , dmaxFrag       = Nothing
  , dGCTable       = IM.empty
  , dGCMain        = Nothing
  , dparMaxCopied  = Nothing
  , dparTotCopied  = Nothing
  , dmaxParNThreads = Nothing
  , dsparkTable    = IM.empty
  }

defaultGC :: Timestamp -> RtsGC
defaultGC time = RtsGC
  { gcMode      = ModeInit
  , gcStartTime = time
  , gcGenStat   = IM.empty
  }

emptyGenStat :: GenStat
emptyGenStat = GenStat
  { gcAll      = 0
  , gcPar      = 0
  , gcElapsed  = 0
  , gcMaxPause = 0
  }

scanEvents :: SummaryData -> CapEvent -> SummaryData
scanEvents !summaryData (CapEvent mcap ev) =
  let -- For events that contain a counter with a running sum.
      -- Eventually we'll subtract the last found
      -- event from the first. Intervals beginning at time 0
      -- are a special case, because morally the first event should have
      -- value 0, but it may be absent, so we start with @Just (0, 0)@.
      alterCounter n Nothing = Just (n, n)
      alterCounter n (Just (_previous, first)) = Just (n, first)
      -- For events that contain discrete increments. We assume the event
      -- is emitted close to the end of the process it measures,
      -- so we ignore the first found event, because most of the process
      -- could have happened before the start of the current inverval.
      -- This is consistent with @alterCounter@. For interval beginning
      -- at time 0, we start with @Just 0@.
      alterIncrement _ Nothing = Just 0
      alterIncrement n (Just k) = Just (k + n)
      -- For events that contain sampled values, where a max is sought.
      alterMax n Nothing = Just n
      alterMax n (Just k) | n > k = Just n
      alterMax _ jk = jk
      -- Scan events, updating summary data.
      scan cap !sd@SummaryData{..} Event{time, spec} =
        let capGC = IM.findWithDefault (defaultGC time) cap dGCTable
        in case spec of
          -- TODO: check EventBlock elsewhere; define {map,fold}EventBlock
          EventBlock{cap = bcap, block_events} ->
            L.foldl' (scan bcap) sd block_events
          HeapAllocated{allocBytes} ->
            sd { dallocTable =
                   IM.alter (alterCounter allocBytes) cap dallocTable }
          HeapLive{liveBytes} ->
            sd { dmaxResidency = alterMax liveBytes dmaxResidency}
          HeapSize{sizeBytes} ->
            sd { dmaxMemory = alterMax sizeBytes dmaxMemory}
          StartGC ->
            assert (gcMode capGC `elem` [ModeInit, ModeEnd, ModeIdle]) $
            let newGC = capGC { gcMode = ModeStart
                              , gcStartTime = time
                              }
            -- TODO: Index with generations, not caps?
            in sd { dGCTable = IM.insert cap newGC dGCTable }
          GlobalSyncGC ->
            -- All caps must be stopped. Those that take part in the GC
            -- are in ModeInit or ModeStart, those that do not
            -- are in ModeInit, ModeEnd or ModeIdle.
            assert (L.all (notModeGHCEtc . gcMode) (IM.elems dGCTable)) $
            sd { dGCTable = IM.mapWithKey setSync dGCTable }
             where
              notModeGHCEtc ModeGHC{}  = False
              notModeGHCEtc ModeSync{} = False
              notModeGHCEtc _          = True
              someInit = L.any ((== ModeInit) . gcMode) (IM.elems dGCTable)
              setSync capKey dGC@RtsGC{gcGenStat}
                | someInit =
                -- If even one cap could possibly have started GC before
                -- the start of the selected interval, skip the GC on all caps.
                -- We don't verify the overwritten modes in this case.
                -- TODO: we could be smarter and defer the decision to EndGC,
                -- when we can deduce if the suspect caps take part in GC
                -- or not at all.
                dGC { gcMode = ModeInit }
                | otherwise =
                let totGC = IM.findWithDefault emptyGenStat gcGenTot gcGenStat
                in case gcMode dGC of
                  -- Cap takes part in the GC (not known if seq or par).
                  -- Here is the moment where all caps taking place in the GC
                  -- are identified and we can aggregate all their data
                  -- at once (currently we just increment a counter for each).
                  -- The EndGC events can come much later for some caps and at
                  -- that time other caps are already inside their new GC.
                  ModeStart ->
                    dGC { gcMode = ModeSync cap
                        , gcGenStat =
                            if capKey == cap
                            then IM.insert gcGenTot
                                   totGC{ gcAll = gcAll totGC + 1 }
                                   gcGenStat
                            else gcGenStat
                        }
                  -- Cap is not in the GC. Mark it as idle to complete
                  -- the identification of caps that take part
                  -- in the current GC. Without overwritin the mode,
                  -- the cap could be processed later on as if
                  -- it took part in the GC, giving wrong results.
                  ModeEnd  -> dGC { gcMode = ModeIdle }
                  ModeIdle -> dGC
                  -- Impossible.
                  ModeInit   -> error "scanEvents: GlobalSyncGC ModeInit"
                  ModeSync{} -> error "scanEvents: GlobalSyncGC ModeSync"
                  ModeGHC{}  -> error "scanEvents: GlobalSyncGC ModeGHC"
          GCStatsGHC{..} ->
            -- All caps must be stopped. Those that take part in the GC
            -- are in ModeInit or ModeSync, those that do not
            -- are in ModeInit or ModeIdle.
            assert (L.all (notModeStartEtc . gcMode) (IM.elems dGCTable)) $
            sd { dcopied  = alterIncrement copied dcopied  -- sum over caps
               , dmaxSlop = alterMax slop dmaxSlop  -- max over all caps
               , dmaxFrag = alterMax frag dmaxFrag  --TODO -- max over all caps
               , dGCTable = IM.mapWithKey setParSeq dGCTable
               , dparMaxCopied = alterIncrement parMaxCopied dparMaxCopied
               , dparTotCopied = alterIncrement parTotCopied dparTotCopied
               , dmaxParNThreads = alterMax parNThreads dmaxParNThreads
               }
             where
              notModeStartEtc ModeStart = False
              notModeStartEtc ModeGHC{} = False
              notModeStartEtc ModeEnd   = False
              notModeStartEtc _         = True
              someInit = L.any ((== ModeInit) . gcMode) (IM.elems dGCTable)
              setParSeq capKey dGC@RtsGC{gcGenStat}
                | someInit =
                -- Just starting the selected interval, so skip the GC.
                dGC
                | otherwise =
                let genGC = IM.findWithDefault emptyGenStat gen gcGenStat
                    totGC = IM.findWithDefault emptyGenStat gcGenTot gcGenStat
                in case gcMode dGC of
                  -- Cap takes part in seq GC.
                  ModeSync capSync | parNThreads == 1 ->
                    assert (cap == capSync) $
                    dGC { gcMode = ModeGHC cap gen
                        , gcGenStat =
                          -- Already inserted into gcGenTot in GlobalSyncGC,
                          -- so only inserting into gen.
                          if capKey == cap
                          then IM.insert gen
                                 genGC{ gcAll = gcAll genGC + 1 }
                                 gcGenStat
                          else gcGenStat
                        }
                  -- Cap takes part in par GC.
                  ModeSync capSync ->
                    assert (cap == capSync) $
                    assert (parNThreads > 1) $
                    dGC { gcMode = ModeGHC cap gen
                        , gcGenStat =
                          if capKey == cap
                          then IM.insert gen
                                 genGC{ gcAll = gcAll genGC + 1
                                      , gcPar = gcPar genGC + 1
                                      }
                                 (IM.insert gcGenTot
                                   -- Already incremented gcAll in SyncGC.
                                   totGC{ gcPar = gcPar totGC + 1 }
                                   gcGenStat)
                          else gcGenStat
                        }
                  -- Cap not in the current GC, leave it alone.
                  ModeIdle -> dGC
                  -- Impossible.
                  ModeInit  -> error "scanEvents: GCStatsGHC ModeInit"
                  ModeStart -> error "scanEvents: GCStatsGHC ModeStart"
                  ModeGHC{} -> error "scanEvents: GCStatsGHC ModeGHC"
                  ModeEnd   -> error "scanEvents: GCStatsGHC ModeEnd"
          EndGC ->
            assert (gcMode capGC `notElem` [ModeStart, ModeEnd, ModeIdle]) $
            let endedGC = capGC { gcMode = ModeEnd }
                duration = time - gcStartTime capGC
                timeGC gen gstat =
                  let genGC =
                        IM.findWithDefault emptyGenStat gen (gcGenStat gstat)
                      newGenGC =
                        genGC { gcElapsed = gcElapsed genGC + duration
                              , gcMaxPause = max (gcMaxPause genGC) duration
                              }
                  in gstat { gcGenStat = IM.insert gen newGenGC
                                             (gcGenStat gstat) }
                timeGenTot = timeGC gcGenTot endedGC
                updateMainCap mainCap _          sdi | mainCap /= cap = sdi
                updateMainCap _       currentGen sdi =
                  -- We are at the EndGC event of the main cap of current GC.
                  -- The timings from this cap are the only that +RTS -s uses.
                  -- Let's record them in the dGCMain field to able
                  -- to display a look-alike of +RTS -s.
                  let dgm = fromMaybe (defaultGC time) dGCMain
                  in sdi { dGCMain = Just $ timeGC currentGen dgm }
            in case gcMode capGC of
                 -- We don't know the exact timing of this GC started before
                 -- the selected interval, so we skip it and clear its mode.
                 ModeInit -> sd {dGCTable = IM.insert cap endedGC dGCTable}
                 -- There is no GCStatsGHC for this GC. Cope without.
                 ModeSync mainCap ->
                   let sdMain = updateMainCap mainCap gcGenTot sd
                   in sdMain {dGCTable = IM.insert cap timeGenTot dGCTable}
                 -- All is known, so we update the times.
                 ModeGHC mainCap gen ->
                   let timeGenCurrent = timeGC gen timeGenTot
                       sdMain = updateMainCap mainCap gen $
                                  updateMainCap mainCap gcGenTot sd
                   in sdMain {dGCTable = IM.insert cap timeGenCurrent dGCTable}
                 _ -> error "scanEvents: ModeEnd (impossible gcMode)"
          SparkCounters crt dud ovf cnv fiz gcd _rem ->
            -- We are guranteed the first spark counters event has all zeroes,
            -- do we don't need to rig the counters for maximal interval.
            let current = RtsSpark crt dud ovf cnv fiz gcd
            in sd { dsparkTable =
                      IM.alter (alterCounter current) cap dsparkTable }
          _ -> sd
    in scan (fromJust mcap) summaryData ev

ppWithCommas :: Word64 -> String
ppWithCommas =
  let spl [] = []
      spl l  = let (c3, cs) = L.splitAt 3 l
               in c3 : spl cs
  in L.reverse . L.intercalate "," . spl . L.reverse . show

printW :: String -> Maybe Word64 -> [String]
printW _ Nothing = []
printW s (Just w) = [printf s $ ppWithCommas w]

memLines :: SummaryData -> [String]
memLines SummaryData{..} =
  printW "%16s bytes allocated in the heap"
    (Just $ L.sum $ L.map (uncurry (-)) $ IM.elems dallocTable) ++
  printW "%16s bytes copied during GC" dcopied ++
  printW "%16s bytes maximum residency" dmaxResidency ++
  printW "%16s bytes maximum slop" dmaxSlop ++
  printf ("%16d MB total memory in use "
          ++ printf "(%d MB lost due to fragmentation)"
                (fromMaybe 0 dmaxFrag `div` (1024 * 1024)))
         (fromMaybe 0 dmaxMemory `div` (1024 * 1024)) :
  [""]

timeToSecondsDbl :: Integral a => a -> Double
timeToSecondsDbl t = fromIntegral t / tIME_RESOLUTION
 where tIME_RESOLUTION = 1000000

gcLines :: SummaryData -> (Double, [String])
gcLines SummaryData{..} =
  let gens = [0..1]  -- TODO: take it from SummaryData instead of hardwiring
      gathered = map gcGather gens
      gcGather :: Gen -> GenStat
      gcGather gen = gcSum gen $ map gcGenStat $ IM.elems dGCTable
      -- TODO: Consider per-HEC display of GC stats and then use
      -- the values summed over all generations at key gcGenTot.
      -- Also, if there is no GC_STATS_GHC event, we don't have generations
      -- and then let's use gcGenTot, which depends only on GC_GLOBAL_SYNC.
      mainStat = gcGenStat $ fromMaybe (defaultGC 0) dGCMain
      gcSum :: Gen -> [IM.IntMap GenStat] -> GenStat
      gcSum gen l =
        let l_genGC = map (IM.findWithDefault emptyGenStat gen) l
            sumPr proj = sum $ map proj l_genGC
            _maxPr proj = L.maximum $ map proj l_genGC
            _minPr proj = L.minimum $ filter (> 0) $ map proj l_genGC
            -- This would be the most balanced, if event times were accurate.
            _avgPr proj = let vs = filter (> 0) $ map proj l_genGC
                          in sum vs `div` fromIntegral (length vs)
            mainGen = IM.findWithDefault emptyGenStat gen mainStat
        in GenStat (sumPr gcAll) (sumPr gcPar)
                   (gcElapsed mainGen) (gcMaxPause mainGen)
      displayGC :: (Gen, GenStat) -> String
      displayGC (gen, GenStat{..}) =
        let gcElapsedS = timeToSecondsDbl gcElapsed
            gcMaxPauseS = timeToSecondsDbl gcMaxPause
            gcAvgPauseS
              | gcAll == 0 = 0
              | otherwise = gcElapsedS / fromIntegral gcAll
        in printf "  Gen  %d     %5d colls, %5d par      %5.2fs         %3.4fs    %3.4fs" gen gcAll gcPar gcElapsedS gcAvgPauseS gcMaxPauseS
      nThreads = maybe 1 fromIntegral dmaxParNThreads :: Double
      balFigure = 100 * ((maybe 0 fromIntegral dparTotCopied
                          / maybe 1 fromIntegral dparMaxCopied)
                         - 1)
                  / (nThreads - 1)
      balText | nThreads < 2 = []
              | otherwise    = ["", printf "    Parallel GC work balance: %.2f%% (serial 0%%, perfect 100%%)" balFigure]
  in ( timeToSecondsDbl $ sum $ map gcElapsed gathered
     , ["                                    "
       ++ "Tot elapsed time  Avg pause  Max pause"] ++
       map displayGC (zip [0..] gathered) ++
       balText ++
       [""]
     )

sparkLines :: SummaryData -> [String]
sparkLines SummaryData{dsparkTable} =
  let diffSparks (RtsSpark crt1 dud1 ovf1 cnv1 fiz1 gcd1,
                  RtsSpark crt2 dud2 ovf2 cnv2 fiz2 gcd2) =
        RtsSpark (crt1 - crt2) (dud1 - dud2) (ovf1 - ovf2)
                 (cnv1 - cnv2) (fiz1 - fiz2) (gcd1 - gcd2)
      dsparkDiff = IM.map diffSparks dsparkTable
      sparkLine :: Cap -> RtsSpark -> String
      sparkLine k = displaySparkCounter (printf "SPARKS HEC %d" k)
      sparkSum = sumSparkCounters $ IM.elems dsparkDiff
      sumSparkCounters l =
        let sumPr proj = L.sum $ L.map proj l
        in RtsSpark
             (sumPr sparkCreated) (sumPr sparkDud) (sumPr sparkOverflowed)
             (sumPr sparkConverted) (sumPr sparkFizzled) (sumPr sparkGCd)
      displaySparkCounter :: String -> RtsSpark -> String
      displaySparkCounter header RtsSpark{..} =
        printf "  %s: %7d (%7d converted, %7d overflowed, %7d dud, %7d GC'd, %7d fizzled)" header (sparkCreated + sparkDud + sparkOverflowed) sparkConverted sparkOverflowed sparkDud sparkGCd sparkFizzled
  in IM.elems (IM.mapWithKey sparkLine dsparkDiff) ++
     [displaySparkCounter "SPARKS TOTAL" sparkSum] ++
     [""]

summaryViewProcessEvents :: Maybe Interval -> Maybe (Array Int CapEvent)
                         -> (String, Maybe (Array Int CapEvent))
summaryViewProcessEvents _ Nothing = ("", Nothing)
summaryViewProcessEvents minterval (Just events) =
  let start =
        if istart == 0
        then -- Intervals beginning at time 0 are a special case,
             -- because morally the first event should have value 0,
             -- but it may be absent, so we start with 0.
             emptySummaryData
               { dallocTable = -- a hack: we assume no more than 999 caps
                   IM.fromDistinctAscList $ zip [0..999] $ repeat (0, 0)
               , dcopied = Just 0
               }
        else emptySummaryData
      eventBlockEnd e | EventBlock{ end_time=t } <- spec $ ce_event e = t
      eventBlockEnd e = time $ ce_event e
      -- Warning: stack overflow when done like in ReadEvents.hs:
      fx acc e = max acc (eventBlockEnd e)
      lastTx = L.foldl' fx 1 (elems $ events)
      (istart, iend) = fromMaybe (0, lastTx) minterval
      f summaryData CapEvent{ce_event=Event{time}}
        | time < istart || time > iend = summaryData
      f summaryData ev = scanEvents summaryData ev
      sd@SummaryData{..} = L.foldl' f start $ elems $ events
      totalElapsed = timeToSecondsDbl $ iend - istart
      (gcTotalElapsed, gcLines_sd) = gcLines sd
      mutElapsed = totalElapsed - gcTotalElapsed
      totalAllocated =
        fromIntegral $ L.sum $ L.map (uncurry (-)) $ IM.elems dallocTable
      allocRate = ppWithCommas $ truncate $ totalAllocated / mutElapsed
      timeLines =
        [ printf "  MUT     time  %6.2fs elapsed" mutElapsed
        , printf "  GC      time  %6.2fs elapsed" gcTotalElapsed
        , printf "  Total   time  %6.2fs elapsed" totalElapsed
        , ""
        , printf "  Alloc rate    %s bytes per elapsed MUT second" allocRate
        , ""
        , printf "  Productivity %.1f%% of MUT elapsed vs total elapsed" $
            mutElapsed * 100 / totalElapsed
        ]
      info = unlines $ memLines sd ++ gcLines_sd ++ sparkLines sd ++ timeLines
  in (info, Just events)

summaryViewSetEvents :: SummaryView -> Maybe (Array Int CapEvent) -> IO ()
summaryViewSetEvents = genericSetEvents (summaryViewProcessEvents Nothing)
