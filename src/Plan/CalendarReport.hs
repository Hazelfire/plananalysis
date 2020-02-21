module Plan.CalendarReport where

import Plan.TaskTypes (PlanTopic(..), Event(..))
import qualified Data.Text as T
import Data.List (sortOn)

showEvent :: Event -> T.Text
showEvent event = eventName event <> (T.pack $ " (" <> show (eventStarting event) <> " to " <> show (eventEnding event) <> ")\n" )

calendarReport :: [PlanTopic] -> T.Text
calendarReport topics =
  let events = concat $ map topicEvents topics
      sortedEvents = sortOn eventStarting events
  in T.concat $ map showEvent sortedEvents
