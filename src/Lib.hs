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
import Data.Text.IO as T
import Data.Text (Text)
import System.Environment (getArgs)
import Plan.PlanTypes (DayPlan(..), CommitmentStatus(..), Commitment(..), dayPlanType)
import Plan.TaskTypes (planTopicType)
import Plan.ResolutionReports (resolutionReport)
import Plan.FinanceReports (expenseReport)
import Plan.CalendarReport (calendarReport)


integrity :: DayPlan -> Double
integrity plan = 
  let successfulTasks = length (filter ((==Success) . commitmentStatus) (planSchedule plan))
      allTasks = length $ planSchedule plan
  in fromRational $ toRational successfulTasks / toRational allTasks


runPlanCommand :: [String] -> IO (Maybe Text)
runPlanCommand args = 
  let function = case args of
        ["commitmentCount"] -> Just $ D.pretty . D.embed D.inject . length . map planSchedule
        ["integrity"] -> Just $ D.pretty . D.embed D.inject  .  map integrity
        ["resolutions"] -> Just $ D.pretty . D.embed D.inject . resolutionReport
        ["expenses"] -> Just $ D.pretty . D.embed D.inject . expenseReport
        _ -> Nothing
  in do
    case function of
      Just func -> do
        dayPlans <- D.input (D.list dayPlanType) =<< T.getContents
        return . Just $ func dayPlans
      Nothing -> return Nothing


runTaskCommand :: [String] -> IO (Maybe Text)
runTaskCommand args = 
  let function = case args of
        ["calendar"] -> Just $ D.pretty . D.embed D.inject . calendarReport
        _ -> Nothing
  in do
    case function of
      Just func -> do
        planTopics <- D.input (D.list planTopicType) =<< T.getContents
        return . Just $ func planTopics
      Nothing -> return Nothing


someFunc :: IO ()
someFunc = do
  args <- getArgs
  result <- runPlanCommand args
  case result of
    Just r -> T.putStrLn r
    Nothing -> do
      result2 <- runTaskCommand args
      case result2 of
        Just r2 -> T.putStrLn r2
        Nothing -> T.putStrLn "Usage: plananalysis (commitmentCount|integrity|resolutions|expenses|calendar)"
    
