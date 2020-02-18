{-
 - This module mainly does parsing and procesing for my dhall plan files
 -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Lib
    ( someFunc
    ) where

import qualified Dhall as D
import qualified Dhall.Core as D
import qualified Dhall.Pretty                    as D
import Data.Text (Text, pack)
import Data.Text.IO as T
import System.Environment (getArgs)
import Plan.PlanTypes (DayPlan(..), CommitmentStatus(..), FailNotes(..), Commitment(..), isFail, dayPlanType)
import Plan.ResolutionReports (resolutionReport)


integrity :: DayPlan -> Double
integrity plan = 
  let successfulTasks = length (filter ((==Success) . commitmentStatus) (planSchedule plan))
      allTasks = length $ planSchedule plan
  in fromRational $ toRational successfulTasks / toRational allTasks

resolutions :: DayPlan -> [D.Natural]
resolutions plan = 
  let schedule = planSchedule plan
      failures = filter (isFail . commitmentStatus) schedule
      mapResolutions = \case
        (Fail (FailNotes _ resolutions)) -> resolutions
        _ -> []
   in concat $ map (mapResolutions . commitmentStatus) schedule

someFunc :: IO ()
someFunc = do
  args <- getArgs
  dayPlans <- D.input (D.list dayPlanType) =<< T.getContents
  let result = case args of
          ["commitmentCount"] -> D.pretty . D.embed D.inject . length $ map planSchedule dayPlans
          ["integrity"] -> D.pretty . D.embed D.inject $  map integrity dayPlans
          ["resolutions"] -> D.pretty . D.embed D.inject  $ resolutionReport dayPlans
          _ -> "Usage: plananalysis (commitmentCount|integrity|resolutions)"
  T.putStrLn (result)

