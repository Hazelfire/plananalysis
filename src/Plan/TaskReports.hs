
module Plan.TaskReports (taskReport, planTaskReport) where

import Plan.TaskTypes (PlanTopic(..), PlanTask(..))
import qualified Data.Text as T
import Data.List (sortOn)
import Data.Maybe (isJust)


planTasksToText :: [PlanTask] -> T.Text
planTasksToText tasks =
  let sortedTasks = sortOn taskStarting tasks
      names = map taskName sortedTasks
   in T.unlines names


taskReport :: [PlanTopic] -> T.Text
taskReport topics = 
  let tasks = concat $ map topicTasks topics
      incompleteTasks = filter (not . isJust . taskEnding) tasks
  in planTasksToText incompleteTasks

planTaskReport :: [PlanTopic] -> T.Text
planTaskReport topics = 
  let tasks = concat $ map topicPlanningTasks topics
      incompleteTasks = filter (not . isJust . taskEnding) tasks
  in planTasksToText incompleteTasks

