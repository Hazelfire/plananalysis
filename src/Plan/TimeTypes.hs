{-# LANGUAGE OverloadedStrings #-}

module Plan.TimeTypes (Date(..), Time(..), timeType, dateType, datetimeType, DateTime(..)) where

import qualified Dhall as D
import Data.Ord (Ordering(..))

data Date = Date {
  dateDay :: Integer,
  dateMonth :: Integer,
  dateYear :: Integer
}
  deriving (Eq)

instance Ord Date where
  compare a b =
    let diff = roughDateToDays a - roughDateToDays b
        result
          | diff == 0 = EQ
          | diff < 0 = LT
          | otherwise = GT
    in result
   where
    roughDateToDays :: Date -> Integer
    roughDateToDays date = dateDay date + dateMonth date * 35 + dateYear date * 12

instance Show Date where
  show d = show (dateDay d) <> "/" <> show (dateMonth d)<> "/" <> show (dateYear d)

dateType :: D.Type Date
dateType = D.record (
  Date <$> (toInteger <$> D.field "day" D.natural)
       <*> (toInteger <$> D.field "month" D.natural)
       <*> (toInteger <$> D.field "year" D.natural)
  )

data Time = Time {
  timeHour :: Integer,
  timeMinute :: Integer
}
  deriving (Eq)

timeToMinutes :: Time -> Integer
timeToMinutes (Time hour minute) = hour * 60 + minute

instance Ord Time where
  compare a b = 
    let diff = timeToMinutes a - timeToMinutes b
        result
          | diff == 0 = EQ
          | diff < 0 = LT
          | otherwise = GT
    in result
  
instance Show Time where
  show t = show (timeHour t) <> ":" <> show (timeMinute t)

timeType :: D.Type Time
timeType = D.record (
  Time . toInteger 
    <$> D.field "hour" D.natural
    <*> (toInteger <$> D.field "minute" D.natural)
  )

data DateTime = DateTime {
  datetimeDate :: Date,
  datetimeTime :: Time
}
  deriving (Eq)

instance Ord DateTime where
  compare a b =
    case compare (datetimeDate a) (datetimeDate b) of
      GT -> GT
      LT -> LT
      EQ -> compare (datetimeTime a) (datetimeTime b)
        

instance Show DateTime where
  show dt = show (datetimeDate dt) <> " " <> show (datetimeTime dt)

datetimeType :: D.Type DateTime
datetimeType = D.record (
  DateTime <$> D.field "date" dateType
           <*> D.field "time" timeType
  )
