module GUI.SummaryView (
    SummaryView,
    summaryViewNew,
    summaryViewSetEvents,
    summaryViewSetInterval,
  ) where

import GHC.RTS.Events

import GUI.Types

import Graphics.UI.Gtk

import Data.Array
import Data.IORef
import Data.Maybe
import Data.Word (Word64)
import Data.List as L
import qualified Data.IntMap as IM
import Control.Monad
import Control.Exception (assert)
import Numeric (showFFloat)
import Text.Printf

------------------------------------------------------------------------------

type Events = Array Int CapEvent

data SummaryView = SummaryView {

    -- we cache the stats for the whole interval
    cacheEventsStats      :: !(IORef (Maybe (Events, SummaryStats, Bool)))

    -- widgets for time stuff
  , labelTimeTotal        :: Label
  , labelTimeMutator      :: Label
  , labelTimeGC           :: Label
  , labelTimeProductivity :: Label

    -- widgets for heap stuff
  , labelHeapMaxSize
  , labelHeapMaxResidency
  , labelHeapAllocTotal
  , labelHeapAllocRate
  , labelHeapMaxSlop      :: (Label, Label, Label, Label)
  , tableHeap             :: Widget

    -- widgets for GC stuff
  , labelGcCopied         :: (Label, Label, Label, Label)
  , labelGcParWorkBalance :: Label
  , storeGcStats          :: ListStore GcStatsEntry
  , tableGc               :: Widget

    -- widgets for sparks stuff
  , storeSparkStats       :: ListStore (Cap, SparkCounts)
  }

------------------------------------------------------------------------------

summaryViewNew :: Builder -> IO SummaryView
summaryViewNew builder = do
    cacheEventsStats <- newIORef Nothing

    let getWidget cast = builderGetObject builder cast
        getLabel       = getWidget castToLabel
        getHeapLabels w1 w2 w3 w4 = liftM4 (,,,) (getLabel w1) (getLabel w2)
                                                 (getLabel w3) (getLabel w4)

    labelTimeTotal        <- getWidget castToLabel "labelTimeTotal"
    labelTimeMutator      <- getWidget castToLabel "labelTimeMutator"
    labelTimeGC           <- getWidget castToLabel "labelTimeGC"
    labelTimeProductivity <- getWidget castToLabel "labelTimeProductivity"


    labelHeapMaxSize      <- getHeapLabels "labelHeapMaxSize"           "labelHeapMaxSizeUnit"
                                           "labelHeapMaxSizeBytes"      "labelHeapMaxSizeUnit1"
    labelHeapMaxResidency <- getHeapLabels "labelHeapMaxResidency"      "labelHeapMaxResidencyUnit"
                                           "labelHeapMaxResidencyBytes" "labelHeapMaxResidencyUnit1"
    labelHeapAllocTotal   <- getHeapLabels "labelHeapAllocTotal"        "labelHeapAllocTotalUnit"
                                           "labelHeapAllocTotalBytes"   "labelHeapAllocTotalUnit1"
    labelHeapAllocRate    <- getHeapLabels "labelHeapAllocRate"         "labelHeapAllocRateUnit"
                                           "labelHeapAllocRateBytes"    "labelHeapAllocRateUnit1"
    labelHeapMaxSlop      <- getHeapLabels "labelHeapMaxSlop"           "labelHeapMaxSlopUnit"
                                           "labelHeapMaxSlopBytes"      "labelHeapMaxSlopUnit1"
    tableHeap             <- getWidget castToWidget "tableHeap"

    labelGcCopied         <- getHeapLabels "labelGcCopied"      "labelGcCopiedUnit"
                                           "labelGcCopiedBytes" "labelGcCopiedUnit1"
    labelGcParWorkBalance <- getWidget castToLabel "labelGcParWorkBalance"
    storeGcStats          <- listStoreNew []
    tableGc               <- getWidget castToWidget "tableGC"

    storeSparkStats       <- listStoreNew []

    let summaryView = SummaryView{..}

    treeviewGcStats <- getWidget castToTreeView "treeviewGcStats"
    treeViewSetModel treeviewGcStats storeGcStats
    let addGcColumn = addColumn treeviewGcStats storeGcStats
    addGcColumn "Generation" $ \(GcStatsEntry gen _ _ _ _ _) ->
      [ cellText := if gen == -1 then "GC Total" else "Gen " ++ show gen ]
    addGcColumn "Collections"     $ \(GcStatsEntry _ colls _ _ _ _) ->
      [ cellText := show colls ]
    addGcColumn "Par collections" $ \(GcStatsEntry _ _ pcolls _ _ _) ->
      [ cellText := show pcolls ]
    addGcColumn "Elapsed time"    $ \(GcStatsEntry _ _ _ time _ _) ->
      [ cellText := printf "%5.2fs" (timeToSecondsDbl time) ]
    addGcColumn "Avg pause"       $ \(GcStatsEntry _ _ _ _ avgpause _) ->
      [ cellText := printf "%3.4fs" avgpause ]
    addGcColumn "Max pause"       $ \(GcStatsEntry _ _ _ _ _ maxpause) ->
      [ cellText := printf "%3.4fs" maxpause ]

    treeviewSparkStats <- getWidget castToTreeView "treeviewSparkStats"
    treeViewSetModel treeviewSparkStats storeSparkStats
    let addSparksColumn = addColumn treeviewSparkStats storeSparkStats
    addSparksColumn "HEC" $ \(hec, _) ->
      [ cellText := if hec == -1 then "Total" else "HEC " ++ show hec ]
    addSparksColumn "Total" $ \(_, SparkCounts total _ _ _ _ _) ->
      [ cellText := show total ]
    addSparksColumn "Converted" $ \(_, SparkCounts _ conv _ _ _ _) ->
      [ cellText := show conv ]
    addSparksColumn "Overflowed" $ \(_, SparkCounts _ _ ovf _ _ _) ->
      [ cellText := show ovf ]
    addSparksColumn "Dud" $ \(_, SparkCounts _ _ _ dud _ _) ->
      [ cellText := show dud ]
    addSparksColumn "GC'd" $ \(_, SparkCounts _ _ _ _ gc _) ->
      [ cellText := show gc ]
    addSparksColumn "Fizzled" $ \(_, SparkCounts _ _ _ _ _ fiz) ->
      [ cellText := show fiz ]

    return summaryView

  where
    addColumn view store title mkAttrs = do
      col  <- treeViewColumnNew
      cell <- cellRendererTextNew
      treeViewColumnSetTitle col title
      treeViewColumnPackStart col cell False
      treeViewAppendColumn view col
      cellLayoutSetAttributes col cell store mkAttrs


------------------------------------------------------------------------------

summaryViewSetEvents :: SummaryView -> Maybe (Array Int CapEvent) -> IO ()
summaryViewSetEvents view@SummaryView{cacheEventsStats} Nothing = do
    writeIORef cacheEventsStats Nothing
    setSummaryStatsEmpty view

summaryViewSetEvents view@SummaryView{cacheEventsStats} (Just events) = do
    let stats = summaryStats events Nothing
      -- this is an almost certain indicator that there
      -- are no heap events in the eventlog:
        hasHeapEvents = heapMaxSize (summHeapStats stats) /= Just 0
    writeIORef cacheEventsStats (Just (events, stats, hasHeapEvents))
    setSummaryStats view stats hasHeapEvents


summaryViewSetInterval :: SummaryView -> Maybe Interval -> IO ()
summaryViewSetInterval view@SummaryView{cacheEventsStats} Nothing = do
    cache <- readIORef cacheEventsStats
    case cache of
      Nothing                  -> return ()
      Just (_, stats, hasHeap) -> setSummaryStats view stats hasHeap

summaryViewSetInterval view@SummaryView{cacheEventsStats} (Just interval) = do
    cache <- readIORef cacheEventsStats
    case cache of
      Nothing                   -> return ()
      Just (events, _, hasHeap) -> setSummaryStats view stats hasHeap
        where stats = summaryStats events (Just interval)

------------------------------------------------------------------------------

setSummaryStats :: SummaryView -> SummaryStats -> Bool -> IO ()
setSummaryStats view SummaryStats{..} hasHeapEvents = do
    setTimeStats  view summTimeStats
    if hasHeapEvents
      then do setHeapStatsAvailable view True
              setHeapStats  view summHeapStats
              setGcStats    view summGcStats
      else    setHeapStatsAvailable view False
    setSparkStats view summSparkStats

setTimeStats :: SummaryView -> TimeStats -> IO ()
setTimeStats SummaryView{..} TimeStats{..} =
  mapM_ (\(label, text) -> set label [ labelText := text ])
    [ (labelTimeTotal       , showFFloat (Just 2) (timeToSecondsDbl timeTotal) "s")
    , (labelTimeMutator     , showFFloat (Just 2) (timeToSecondsDbl timeMutator) "s")
    , (labelTimeGC          , showFFloat (Just 2) (timeToSecondsDbl timeGC) "s")
    , (labelTimeProductivity, showFFloat (Just 1) (timeProductivity * 100) "% of mutator vs total")
    ]

setHeapStats :: SummaryView -> HeapStats -> IO ()
setHeapStats SummaryView{..} HeapStats{..} = do
    setHeapStatLabels labelHeapMaxSize      heapMaxSize      "" ""
    setHeapStatLabels labelHeapMaxResidency heapMaxResidency "" ""
    setHeapStatLabels labelHeapAllocTotal   heapTotalAlloc   "" ""
    setHeapStatLabels labelHeapAllocRate    heapAllocRate    "/s" " per second (of mutator time)"
    setHeapStatLabels labelHeapMaxSlop      heapMaxSlop      "" ""
    setHeapStatLabels labelGcCopied         heapCopiedDuringGc "" ""
  where
    setHeapStatLabels labels stat unitSuffix unitSuffixLong =
      let texts = case stat of
            Nothing -> ("N/A", "", "", "")
            Just b  -> ( formatBytesInUnit b u, formatUnit u ++ unitSuffix
                       , formatBytes b, "bytes" ++ unitSuffixLong)
              where u = getByteUnit b
      in setLabels labels texts

    setLabels (short,shortunit,long,longunit) (short', shortunit', long', longunit') = do
      mapM_ (\(label, text) -> set label [ labelText := text ])
            [ (short, short'), (shortunit, shortunit')
            , (long, long'),   (longunit, longunit') ]


setGcStats :: SummaryView -> GcStats -> IO ()
setGcStats SummaryView{..} GcStats{..} = do
  let balText = maybe "N/A"
                      (printf "%.2f%% (serial 0%%, perfect 100%%)")
                      gcParWorkBalance
  set labelGcParWorkBalance [ labelText := balText ]
  listStoreClear storeGcStats
  mapM_ (listStoreAppend storeGcStats) (gcTotalStats:gcGenStats)

setSparkStats :: SummaryView -> SparkStats -> IO ()
setSparkStats SummaryView{..} SparkStats{..} = do
  listStoreClear storeSparkStats
  mapM_ (listStoreAppend storeSparkStats) ((-1,totalSparkStats):capSparkStats)

data ByteUnit = TiB | GiB | MiB | KiB | B deriving Show

byteUnitVal :: ByteUnit -> Word64
byteUnitVal TiB = 2^40
byteUnitVal GiB = 2^30
byteUnitVal MiB = 2^20
byteUnitVal KiB = 2^10
byteUnitVal   B = 1

getByteUnit :: Word64 -> ByteUnit
getByteUnit b
  | b >= 2^40 = TiB
  | b >= 2^30 = GiB
  | b >= 2^20 = MiB
  | b >= 2^10 = KiB
  | otherwise = B

formatBytesInUnit :: Word64 -> ByteUnit -> String
formatBytesInUnit n u =
    formatFixed (fromIntegral n / fromIntegral (byteUnitVal u))
  where
    formatFixed x = showFFloat (Just 1) x ""

formatUnit :: ByteUnit -> String
formatUnit = show

formatBytes :: Word64 -> String
formatBytes b = ppWithCommas b

ppWithCommas :: Word64 -> String
ppWithCommas =
  let spl [] = []
      spl l  = let (c3, cs) = L.splitAt 3 l
               in c3 : spl cs
  in L.reverse . L.intercalate "," . spl . L.reverse . show

setSummaryStatsEmpty :: SummaryView -> IO ()
setSummaryStatsEmpty SummaryView{..} = do
  mapM_ (\label -> set label [ labelText := "", widgetTooltipText := Nothing ]) $
    [ labelTimeTotal, labelTimeMutator
    , labelTimeGC, labelTimeProductivity ] ++
    [ w
    | (a,b,c,d) <- [ labelHeapMaxSize, labelHeapMaxResidency
                   , labelHeapAllocTotal, labelHeapAllocRate
                   , labelHeapMaxSlop, labelGcCopied ]
    , w <- [ a,b,c,d] ]
  listStoreClear storeGcStats
  listStoreClear storeSparkStats

setHeapStatsAvailable :: SummaryView -> Bool -> IO ()
setHeapStatsAvailable SummaryView{..} available
  | available = do
      forM_ unavailableWidgets $ \widget ->
        set widget [ widgetTooltipText := Nothing, widgetSensitive := True ]

  | otherwise = do
      forM_ allLabels $ \label -> set label [ labelText := "" ]
      listStoreClear storeGcStats

      forM_ unavailableLabels  $ \label  ->
        set label  [ labelText := "(unavailable)" ]

      forM_ unavailableWidgets $ \widget ->
        set widget [ widgetTooltipText := Just msgInfoUnavailable, widgetSensitive := False ]

  where
    allLabels =
      [ labelTimeMutator, labelTimeGC
      , labelTimeProductivity, labelGcParWorkBalance ] ++
      [ w | (a,b,c,d) <- [ labelHeapMaxSize, labelHeapMaxResidency
                         , labelHeapAllocTotal, labelHeapAllocRate
                         , labelHeapMaxSlop, labelGcCopied ]
          , w <- [ a,b,c,d] ]
    unavailableLabels =
      [ labelTimeMutator, labelTimeGC
      , labelTimeProductivity, labelGcParWorkBalance
      , case labelGcCopied of (w,_,_,_) -> w ] ++
      [ c | (_,_,c,_) <- [ labelHeapMaxSize, labelHeapMaxResidency
                         , labelHeapAllocTotal, labelHeapAllocRate
                         , labelHeapMaxSlop ] ]
    unavailableWidgets = [ toWidget labelTimeMutator, toWidget labelTimeGC
                         , toWidget labelTimeProductivity
                         , tableHeap, tableGc ]
    msgInfoUnavailable = "This eventlog does not contain heap or GC information."

------------------------------------------------------------------------------
-- Calculating the stats we want to display
--

data SummaryStats = SummaryStats {
       summTimeStats  :: TimeStats,
       summHeapStats  :: HeapStats,
       summGcStats    :: GcStats,
       summSparkStats :: SparkStats
     }

data TimeStats = TimeStats {
       timeTotal        :: !Word64, -- we really should have a better type for elapsed time
       timeGC           :: !Word64,
       timeMutator      :: !Word64,
       timeProductivity :: !Double
     }

data HeapStats = HeapStats {
       heapMaxSize        :: Maybe Word64,
       heapMaxResidency   :: Maybe Word64,
       heapMaxSlop        :: Maybe Word64,
       heapTotalAlloc     :: Maybe Word64,
       heapAllocRate      :: Maybe Word64,
       heapCopiedDuringGc :: Maybe Word64
     }

data GcStats = GcStats {
       gcNumThreads     :: !Int,
       gcParWorkBalance :: !(Maybe Double),
       gcGenStats       :: [GcStatsEntry],
       gcTotalStats     :: !GcStatsEntry
     }
data GcStatsEntry = GcStatsEntry !Int !Int !Int !Word64 !Double !Double

data SparkStats = SparkStats {
       capSparkStats   :: [(Cap, SparkCounts)],
       totalSparkStats :: !SparkCounts
     }
data SparkCounts = SparkCounts !Word64 !Word64 !Word64 !Word64 !Word64 !Word64


-- | Take the events, and optionally some sub-range, and generate the summary
-- stats for that range.
--
-- We take a two-step approach:
--  * a single pass over the events, accumulating into an intermediate
--    'StatsAccum' record,
--  * then look at that 'StatsAccum' record and construct the various final
--    stats that we want to present.
--
summaryStats :: Array Int CapEvent -> Maybe Interval -> SummaryStats
summaryStats events minterval =
    SummaryStats {
       summHeapStats  = hs,
       summGcStats    = gs,
       summSparkStats = ss,
       summTimeStats  = ts
     }
  where
    !statsAccum = accumStats events minterval

    gs = gcStats    statsAccum
    ss = sparkStats statsAccum
    ts = timeStats  events minterval gs
    hs = heapStats  statsAccum ts


-- | Linearly accumulate the stats from the events array,
-- either the full thing or some sub-range.
accumStats :: Array Int CapEvent -> Maybe Interval -> StatsAccum
accumStats events minterval =
    foldl' accumEvent start [ events ! i | i <- range eventsRange ]
  where
    eventsRange = selectEventRange events minterval

    -- If we're starting from time zero then we know many of the stats
    -- also start at from, where as from other points it's just unknown
    start | fst eventsRange == 0 = zeroStatsAccum
          | otherwise            = emptyStatsAccum

-- | Given the event array and a time interval, return the range of array
-- indicies containing that interval. The Nothing interval means to select
-- the whole array range.
--
selectEventRange :: Array Int CapEvent -> Maybe Interval -> (Int, Int)
selectEventRange arr Nothing             = bounds arr
selectEventRange arr (Just (start, end)) = (lbound, ubound)
  where
    !lbound = either snd id $ findArrayRange cmp arr start
    !ubound = either fst id $ findArrayRange cmp arr end
    cmp ts (CapEvent _ (Event ts' _)) = compare ts ts'

    findArrayRange :: (key -> val -> Ordering)
                   -> Array Int val -> key -> Either (Int,Int) Int
    findArrayRange cmp arr key =
        binarySearch a0 b0 key
      where
        (a0,b0) = bounds arr

        binarySearch a b key
          | a > b     = Left (b,a)
          | otherwise = case cmp key (arr ! mid) of
              LT -> binarySearch a (mid-1) key
              EQ -> Right mid
              GT -> binarySearch (mid+1) b key
          where mid = (a + b) `div` 2

------------------------------------------------------------------------------
-- Final step where we convert from StatsAccum to various presentation forms

timeStats :: Array Int CapEvent -> Maybe Interval -> GcStats -> TimeStats
timeStats events minterval
          GcStats { gcTotalStats = GcStatsEntry _ _ _ timeGC _ _ } =
    TimeStats {..}
  where
    timeTotal        = intervalEnd - intervalStart
    timeMutator      = timeTotal   - timeGC
    timeProductivity = timeToSecondsDbl timeMutator
                     / timeToSecondsDbl timeTotal

    (intervalStart, intervalEnd) =
      case minterval of
        Just (s,e) -> (s, e)
        Nothing    -> (0, timeOf (events ! ub))
          where
            (_lb, ub) = bounds events
            timeOf (CapEvent _ (Event t _)) = t


heapStats :: StatsAccum -> TimeStats -> HeapStats
heapStats StatsAccum{..} TimeStats{timeMutator} =
    HeapStats {
      heapMaxSize        = dmaxMemory,
      heapMaxResidency   = dmaxResidency,
      heapMaxSlop        = dmaxSlop,
      heapTotalAlloc     = if totalAlloc == 0
                             then Nothing
                             else Just totalAlloc,
      heapAllocRate      = if timeMutator == 0 || totalAlloc == 0
                              then Nothing
                              else Just $ truncate (fromIntegral totalAlloc / timeToSecondsDbl timeMutator),
      heapCopiedDuringGc = if dcopied == Just 0
                              then Nothing
                              else dcopied
    }
  where
    totalAlloc = sum [ end - start
                     | (end,start) <- IM.elems dallocTable ]


gcStats :: StatsAccum -> GcStats
gcStats StatsAccum{..} =
    GcStats {
      gcNumThreads     = nThreads,
      gcParWorkBalance,
      gcGenStats       = [ mkGcStatsEntry gen (gcGather gen)
                         | gen <- gens ],
      gcTotalStats     = mkGcStatsEntry gcGenTot (gcGather gcGenTot)
    }
  where
    nThreads = fromMaybe 1 dmaxParNThreads

    gcParWorkBalance | nThreads <= 1
                       || fromMaybe 0 dparMaxCopied <= 0 = Nothing
                     | otherwise =
      Just $
        100 * ((maybe 0 fromIntegral dparTotCopied
                / maybe 0 fromIntegral dparMaxCopied) - 1)
              / (fromIntegral nThreads - 1)

    gens = [0..maxGeneration]
      where
        -- Does not work for generationless GCs, but works reasonably
        -- for > 2 gens and perfectly for 2 gens.
        maxGeneration = maximum $ 1
                                : [ maxGen
                                  | RtsGC { gcGenStat } <- IM.elems dGCTable
                                  , not (IM.null gcGenStat)
                                  , let (maxGen, _) = IM.findMax gcGenStat ]

    gcGather :: Gen -> GenStat
    gcGather gen = gcSum gen $ map gcGenStat $ IM.elems dGCTable
    -- TODO: Consider per-HEC display of GC stats and then use
    -- the values summed over all generations at key gcGenTot at each cap.

    gcSum :: Gen -> [IM.IntMap GenStat] -> GenStat
    gcSum gen l =
        GenStat (sumPr gcAll) (sumPr gcPar)
                (gcElapsed mainGen) (gcMaxPause mainGen)
      where
        l_genGC = map (IM.findWithDefault emptyGenStat gen) l
        sumPr proj = sum $ map proj l_genGC
        _maxPr proj = L.maximum $ map proj l_genGC
        _minPr proj = L.minimum $ filter (> 0) $ map proj l_genGC
        -- This would be the most balanced way of aggregating gcElapsed,
        -- if only the event times were accurate.
        _avgPr proj = let vs = filter (> 0) $ map proj l_genGC
                      in sum vs `div` fromIntegral (length vs)
        -- But since the times include scheduling noise,
        -- we only use the times from the main cap for each GC
        -- and so get readings almost identical to +RTS -s.
        mainGen = IM.findWithDefault emptyGenStat gen mainStat

    mainStat = gcGenStat (fromMaybe (defaultGC 0) dGCMain)

    mkGcStatsEntry :: Gen -> GenStat -> GcStatsEntry
    mkGcStatsEntry gen GenStat{..} =
        GcStatsEntry gen gcAll gcPar gcElapsedS gcAvgPauseS gcMaxPauseS
      where
        gcElapsedS  = gcElapsed
        gcMaxPauseS = timeToSecondsDbl gcMaxPause
        gcAvgPauseS
          | gcAll == 0 = 0
          | otherwise  = timeToSeconds $
                           fromIntegral gcElapsed / fromIntegral gcAll


sparkStats :: StatsAccum -> SparkStats
sparkStats StatsAccum{dsparkTable} =
    SparkStats {
      capSparkStats =
        [ (cap, mkSparkStats sparkCounts)
        | (cap, sparkCounts) <- capsSparkCounts ],

      totalSparkStats =
        mkSparkStats $
        foldl' (binopSparks (+)) zeroSparks
          [ sparkCounts | (_cap, sparkCounts) <- capsSparkCounts ]
    }
  where
    capsSparkCounts =
      [ (cap,  sparkCounts)
      | (cap, (countsEnd, countsStart)) <- IM.assocs dsparkTable
      , let sparkCounts = binopSparks (-) countsEnd countsStart ]

    mkSparkStats RtsSpark {sparkCreated, sparkDud, sparkOverflowed,
                           sparkConverted, sparkFizzled, sparkGCd} =
      -- in our final presentation we show the total created,
      -- and the breakdown of that into outcomes:
      SparkCounts (sparkCreated + sparkDud + sparkOverflowed)
                  sparkConverted sparkOverflowed
                  sparkDud sparkGCd sparkFizzled


------------------------------------------------------------------------------

timeToSecondsDbl :: Integral a => a -> Double
timeToSecondsDbl t = timeToSeconds $ fromIntegral t

timeToSeconds :: Double -> Double
timeToSeconds t = t / tIME_RESOLUTION
 where tIME_RESOLUTION = 1000000

------------------------------------------------------------------------------
-- The single-pass stats accumulation stuff
--

-- | Data collected and computed gradually while events are scanned.
data StatsAccum = StatsAccum
  { dallocTable     :: !(IM.IntMap (Word64, Word64))  -- indexed by caps
  , dcopied         :: !(Maybe Word64)
  , dmaxResidency   :: !(Maybe Word64)
  , dmaxSlop        :: !(Maybe Word64)
  , dmaxMemory      :: !(Maybe Word64)
--, dmaxFrag        :: Maybe Word64  -- not important enough
  , dGCTable        :: !(IM.IntMap RtsGC)  -- indexed by caps
  -- Here we store the official +RTS -s timings of GCs,
  -- that is times aggregated from the main caps of all GCs.
  -- For now only gcElapsed and gcMaxPause are needed, so the rest
  -- of the fields stays at default values.
  , dGCMain         :: !(Maybe RtsGC)
  , dparMaxCopied   :: !(Maybe Word64)
  , dparTotCopied   :: !(Maybe Word64)
  , dmaxParNThreads :: !(Maybe Int)
--, dtaskTable      -- of questionable usefulness, hard to get
  , dsparkTable     :: !(IM.IntMap (RtsSpark, RtsSpark))  -- indexed by caps
--, dInitExitT      -- TODO. At least init time can be included in the total
                    -- time registered in the eventlog. Can we measure this
                    -- as the time between some initial events?
--, dGCTime         -- Is better computed after all events are scanned,
                    -- e.g., because the same info can be used to calculate
                    -- per-cap GCTime and other per-cap stats.
--, dtotalTime      -- TODO: can we measure this excluding INIT or EXIT times?
  }

data RtsSpark = RtsSpark
 { sparkCreated, sparkDud, sparkOverflowed
 , sparkConverted, sparkFizzled, sparkGCd :: !Word64
 }

zeroSparks :: RtsSpark
zeroSparks = RtsSpark 0 0 0 0 0 0

binopSparks :: (Word64 -> Word64 -> Word64) -> RtsSpark -> RtsSpark -> RtsSpark
binopSparks op (RtsSpark crt1 dud1 ovf1 cnv1 fiz1 gcd1)
               (RtsSpark crt2 dud2 ovf2 cnv2 fiz2 gcd2) =
      RtsSpark (crt1 `op` crt2) (dud1 `op` dud2) (ovf1 `op` ovf2)
               (cnv1 `op` cnv2) (fiz1 `op` fiz2) (gcd1 `op` gcd2)

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

emptyStatsAccum :: StatsAccum
emptyStatsAccum = StatsAccum
  { dallocTable     = IM.empty
  , dcopied         = Nothing
  , dmaxResidency   = Nothing
  , dmaxSlop        = Nothing
  , dmaxMemory      = Nothing
  , dGCTable        = IM.empty
  , dGCMain         = Nothing
  , dparMaxCopied   = Nothing
  , dparTotCopied   = Nothing
  , dmaxParNThreads = Nothing
  , dsparkTable     = IM.empty
  }

-- | At the beginning of a program run, we know for sure several of the
-- stats start at zero:
zeroStatsAccum :: StatsAccum
zeroStatsAccum = emptyStatsAccum {
    dcopied       = Just 0,
    dmaxResidency = Just 0,
    dmaxSlop      = Just 0,
    dmaxMemory    = Just 0,
    dallocTable   = -- a hack: we assume no more than 999 caps
                    IM.fromDistinctAscList $ zip [0..999] $ repeat (0, 0)
                    -- FIXME: but also, we should have a way to init to 0 for all caps.
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

accumEvent :: StatsAccum -> CapEvent -> StatsAccum
accumEvent !statsAccum (CapEvent mcap ev) =
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
      scan cap !sd@StatsAccum{..} Event{time, spec} =
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
            assert (gcMode capGC `notElem` [ModeEnd, ModeIdle]) $
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
                updateMainCap mainCap _          dgm | mainCap /= cap = dgm
                updateMainCap _       currentGen dgm =
                  -- We are at the EndGC event of the main cap of current GC.
                  -- The timings from this cap are the only that +RTS -s uses.
                  -- We will record them in the dGCMain field to be able
                  -- to display a look-alike of +RTS -s.
                  timeGC currentGen dgm
            in case gcMode capGC of
                 -- We don't know the exact timing of this GC started before
                 -- the selected interval, so we skip it and clear its mode.
                 ModeInit -> sd { dGCTable = IM.insert cap endedGC dGCTable }
                 -- There is no GlobalSyncGC nor GCStatsGHC for this GC.
                 -- Consequently, we can't determine the main cap,
                 -- so skip it and and clear its mode.
                 ModeStart -> sd { dGCTable = IM.insert cap endedGC dGCTable }
                 -- There is no GCStatsGHC for this GC. Gather partial data.
                 ModeSync mainCap ->
                   let dgm = fromMaybe (defaultGC time) dGCMain
                       mainGenTot = updateMainCap mainCap gcGenTot dgm
                   in sd { dGCTable = IM.insert cap timeGenTot dGCTable
                         , dGCMain = Just mainGenTot
                         }
                 -- All is known, so we update the times.
                 ModeGHC mainCap gen ->
                   let newTime = timeGC gen timeGenTot
                       dgm = fromMaybe (defaultGC time) dGCMain
                       mainGenTot = updateMainCap mainCap gcGenTot dgm
                       newMain = updateMainCap mainCap gen mainGenTot
                   in sd { dGCTable = IM.insert cap newTime dGCTable
                         , dGCMain = Just newMain
                         }
                 ModeEnd   -> error "scanEvents: EndGC ModeEnd"
                 ModeIdle  -> error "scanEvents: EndGC ModeIdle"
          SparkCounters crt dud ovf cnv fiz gcd _rem ->
            -- We are guranteed the first spark counters event has all zeroes,
            -- do we don't need to rig the counters for maximal interval.
            let current = RtsSpark crt dud ovf cnv fiz gcd
            in sd { dsparkTable =
                      IM.alter (alterCounter current) cap dsparkTable }
          _ -> sd
    in scan (fromJust mcap) statsAccum ev
