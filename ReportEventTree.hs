-- This is a module used to generate debugging and performance tuning
-- information regarding the event-tree data structure.

module ReportEventTree
where 

import EventlogViewerCommon

-------------------------------------------------------------------------------

reportEventTrees :: HECs -> IO ()
reportEventTrees = mapM_ reportEventTree

-------------------------------------------------------------------------------

reportEventTree :: (Int, EventTree) -> IO ()
reportEventTree (hecNumber, eventTree)
  = putStrLn ("HEC " ++ show hecNumber ++ reportText)
    where
    reportText = " nodes = " ++ show (countNodes eventTree) ++ 
                 " max depth = " ++ show (maxDepth eventTree)

-------------------------------------------------------------------------------

countNodes :: EventTree -> Int
countNodes (EventSplit _ _ _ lhs rhs _) = 1 + countNodes lhs + countNodes rhs
countNodes (EventTreeLeaf _) = 1

-------------------------------------------------------------------------------

maxDepth :: EventTree -> Int
maxDepth (EventSplit _ _ _ lhs rhs _)
  = 1 + maxDepth lhs `max` maxDepth rhs
maxDepth (EventTreeLeaf _) = 1

-------------------------------------------------------------------------------
