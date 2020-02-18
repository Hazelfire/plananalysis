{-
 - This module contains reports that can be made for finance
 -}

module Plan.FinanceReports (balanceReport) where

import qualified Dhall as D
import qualified Dhall.Core as D
import Plan.PlanTypes (DayPlan(..), Transaction(..), Currency(..))

currencyToCents :: Currency -> Integer
currencyToCents (Currency cents) = cents

balanceReport 
  :: Integer -- Starting Balance (in cents)
  -> [DayPlan] -- Transactions made (in order for which they were made)
  -> [Integer] -- Cumulative Balance
balanceReport starting (p:ps) = 
  let amountSpent = sum $ map (currencyToCents . transactionAmount) (planTransactions p)
  in
  starting : balanceReport (starting - amountSpent) ps
balanceReport starting [] = [starting]
