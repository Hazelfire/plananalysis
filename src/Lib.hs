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

data Commitment = Commitment {
  commitmentName :: Text,
  commitmentEnding :: Text,
  commitmentStarting :: Text,
  commitmentStatus :: CommitmentStatus
}

commitmentType :: D.Type Commitment
commitmentType = D.record (
  Commitment <$> (D.field "name" D.strictText)
             <*> (D.field "ending" D.strictText) 
             <*> (D.field "starting" D.strictText)
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
  dayPlan <- D.input (D.list dayPlanType) =<< T.getContents
  let result = case args of
          ["commitmentCount"] -> D.pretty . D.embed D.inject . length $ map planSchedule dayPlan
          ["integrity"] -> D.pretty . D.embed D.inject $  map integrity dayPlan
          ["resolutions"] -> D.pretty . D.embed D.inject  $ map resolutions dayPlan
          _ -> "Usage: plananalysis (commitmentCount|integrity|resolutions)"
  T.putStrLn (result)

