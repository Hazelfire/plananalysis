{-
 - Contains all the types required to process daily plans
 -}
{-# LANGUAGE OverloadedStrings #-}

module Plan.PlanTypes ( DayPlan(..)
                      , FailNotes(..)
                      , CommitmentStatus(..)
                      , Commitment(..)
                      , isFail
                      , dayPlanType
                      , Time(..)
                      , Transaction(..)
                      , Currency(..)
                      , Measurement(..)
                      , currencyToCents
                      , MeasurementSet(..)
                      ) where 

import qualified Dhall as D
import Plan.TimeTypes (Time(..), timeType)
import Data.Text (Text)


data FailNotes = FailNotes {
  failReason :: Text,
  failResolutions :: [D.Natural]
}
 deriving (Eq, Show)

failNotesType :: D.Type FailNotes
failNotesType =  
    D.record (FailNotes <$> (D.field "reason" D.strictText) <*> (D.field "resolutions" (D.list D.natural)))

newtype Currency = Currency Integer
 deriving (Eq, Show)

currencyType :: D.Type Currency
currencyType = 
  D.record (Currency . toInteger <$> ( ( (+) . (*100) <$> D.field "dollars" D.natural) <*> D.field "cents" D.natural))

currencyToCents :: Currency -> Integer
currencyToCents (Currency c) = c

data Transaction = Transaction {
  transactionName :: Text,
  transactionAmount :: Currency,
  transactionRebates :: [Rebate],
  transactionClass :: D.Natural
}
  deriving (Eq, Show)

data Rebate = Rebate {
  rebatesFrom :: Text,
  rebateAmount :: Currency
}
  deriving (Eq, Show)

rebateType :: D.Type Rebate
rebateType = 
  D.record (Rebate <$> D.field "from" D.strictText <*> D.field "amount" currencyType)

transactionType :: D.Type Transaction
transactionType = D.record (
  Transaction <$> D.field "name" D.strictText 
              <*> D.field "amount" currencyType
              <*> D.field "rebates" (D.list rebateType)
              <*> D.field "class" D.natural
  )
  
  
data CommitmentStatus = Fail FailNotes | Success | Incomplete
 deriving (Eq, Show)

isFail :: CommitmentStatus -> Bool
isFail (Fail {}) = True
isFail _ = False

commitmentStatusType :: D.Type CommitmentStatus
commitmentStatusType = D.union
  (  (Fail <$> D.constructor "Fail" failNotesType )
  <> (Success <$ D.constructor "Success" D.unit )
  <> (Incomplete <$ D.constructor "Incomplete" D.unit)
  )


data Commitment = Commitment {
  commitmentName :: Text,
  commitmentEnding :: Time,
  commitmentStarting :: Time,
  commitmentStatus :: CommitmentStatus
}

commitmentType :: D.Type Commitment
commitmentType = D.record (
  Commitment <$> (D.field "name" D.strictText)
             <*> (D.field "ending" timeType)
             <*> (D.field "starting" timeType)
             <*> D.field "status" commitmentStatusType)

data Measurement = Measurement
  { measurementMeasure :: D.Natural
  , measurementAmount :: D.Natural
  , measurementResolutions :: [D.Natural]
  }

measurementType :: D.Type Measurement
measurementType = D.record (
  Measurement <$> D.field "measure" D.natural
              <*> D.field "amount" D.natural
              <*> D.field "resolutions" (D.list D.natural)
  )

data MeasurementSet = MeasurementSet
  { msTime :: Time
  , msMeasurements :: [Measurement]
  }

measurementSetType :: D.Type MeasurementSet
measurementSetType = D.record (
  MeasurementSet <$> D.field "time" timeType
                 <*> D.field "measurements" (D.list measurementType)
  )

data DayPlan = DayPlan {
  planDate :: D.Natural,
  planTransactions :: [Transaction],
  planSchedule :: [Commitment],
  planTasks :: [Text],
  planMeasurements :: [MeasurementSet]
}

dayPlanType :: D.Type DayPlan
dayPlanType = D.record (
  DayPlan <$> D.field "date" D.natural
          <*> D.field "finance" (D.list transactionType)
          <*> D.field "schedule" (D.list commitmentType)
          <*> D.field "tasks" (D.list D.strictText)
          <*> D.field "measurements" (D.list measurementSetType)
   )

