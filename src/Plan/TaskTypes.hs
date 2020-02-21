{-# LANGUAGE OverloadedStrings #-}
{-
These types are around the tasks and planning tasks.
-}

module Plan.TaskTypes (PlanTopic(..), PlanTask(..), Event(..), planTaskType, eventType, planTopicType) where

import Dhall as D
import Data.Text (Text) 
import Plan.TimeTypes (datetimeType, DateTime(..))

data PlanTask = PlanTask
  { taskName :: Text
  , taskStarting :: DateTime
  , taskEnding :: Maybe DateTime
  }

planTaskType :: D.Type PlanTask
planTaskType = D.record (
  PlanTask <$> D.field "name" D.strictText
           <*> D.field "started" datetimeType
           <*> D.field "ended" (D.maybe datetimeType)
  )

data Event = Event {
  eventName :: Text,
  eventStarting :: DateTime,
  eventEnding :: DateTime
}

eventType :: D.Type Event
eventType = D.record (
  Event <$> D.field "name" D.strictText
        <*> D.field "starting" datetimeType
        <*> D.field "ending" datetimeType
  )

data PlanTopic = PlanTopic 
  { topicName :: Text  
  , topicPlanningTasks :: [PlanTask]
  , topicTasks :: [PlanTask]
  , topicEvents :: [Event]
  }

planTopicType :: D.Type PlanTopic
planTopicType = D.record (
  PlanTopic <$> D.field "name" D.strictText
            <*> D.field "planningTasks" (D.list planTaskType)
            <*> D.field "tasks" (D.list planTaskType)
            <*> D.field "events" (D.list eventType)
  )
