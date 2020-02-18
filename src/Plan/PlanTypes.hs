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
                      ) where 

import qualified Dhall as D
import qualified Dhall.Core as D
import qualified Dhall.Pretty                    as D
import Data.Text (Text, pack)


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

data Transaction = Transaction {
  transactionName :: Text,
  transactionAmount :: Currency,
  transactionRebates :: [Rebate]
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
transactionType =
  D.record (Transaction <$> D.field "name" D.strictText <*> D.field "amount" currencyType <*> D.field "rebates" (D.list rebateType))
  
  
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

data Time = Time {
  timeHour :: Integer,
  timeMinute :: Integer
}

timeType :: D.Type Time
timeType = D.record (
  Time . toInteger 
    <$> D.field "hour" D.natural
    <*> (toInteger <$> D.field "minute" D.natural)
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

data DayPlan = DayPlan {
  planDate :: D.Natural,
  planTransactions :: [Transaction],
  planSchedule :: [Commitment],
  planTasks :: [Text]
}

dayPlanType :: D.Type DayPlan
dayPlanType = D.record (
  DayPlan <$> D.field "date" D.natural
          <*> D.field "finance" (D.list transactionType)
          <*> D.field "schedule" (D.list commitmentType)
          <*> D.field "tasks" (D.list D.strictText))

