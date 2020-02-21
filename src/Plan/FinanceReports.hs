{-
 - This module contains reports that can be made for finance
 -}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Plan.FinanceReports (expenseReport) where

import qualified Dhall as D
import Plan.PlanTypes (DayPlan(..), Transaction(..), currencyToCents)



data TransactionReport = TransactionReport {
  trDay :: D.Natural,
  trAmount :: Integer,
  trClass :: D.Natural
}
 deriving (D.Generic, D.Inject)



expenseReport 
  :: [DayPlan] -- Transactions made (in order for which they were made)
  -> [TransactionReport]  -- Cumulative Balance
expenseReport ps = concat $ map planToReport ps
 where
  planToReport :: DayPlan -> [TransactionReport]
  planToReport p = map (transactionToReport (planDate p)) (planTransactions p)

  transactionToReport :: D.Natural -> Transaction -> TransactionReport
  transactionToReport day t = TransactionReport day (currencyToCents $ transactionAmount t) (transactionClass t)
