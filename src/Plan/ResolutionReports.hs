{-
 - This module is all about geting reports regarding the resolutions I set
 - myself
 -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Plan.ResolutionReports (resolutionReport) where

import qualified Dhall as D
import qualified Dhall.Core as D
import qualified Dhall.Pretty                    as D
import Plan.PlanTypes (DayPlan(..), FailNotes(..), CommitmentStatus(..), isFail, Commitment(..), Time(..))

data ResolutionReport = ResolutionReport 
  { rrResolution :: D.Natural
  , rrAmount :: D.Natural
  , rrTime :: Integer
  }
  deriving (D.Generic, D.Inject)

commitmentResolutions :: Commitment -> [D.Natural]
commitmentResolutions c = case commitmentStatus c of
     Fail (FailNotes _ resolutions) -> resolutions
     _ -> []

totalCommitment :: [Commitment] -> (D.Natural -> ResolutionReport)
totalCommitment (committment:xs) resolutionNumber = 
  let otherReports = totalCommitment xs
  in
  if elem resolutionNumber (commitmentResolutions committment) then adjustReport  committment (otherReports resolutionNumber) else otherReports resolutionNumber
 where
  adjustReport :: Commitment -> ResolutionReport -> ResolutionReport
  adjustReport commitment (ResolutionReport resNo resAmount resTime) =
    ResolutionReport resNo (resAmount + 1) (resTime + diffTime (commitmentStarting commitment) (commitmentEnding commitment))

  diffTime :: Time -> Time -> Integer
  diffTime (Time ha ma) (Time hb mb) = mb - ma + (hb - ha) * 60

totalCommitment [] resolutionNumber = ResolutionReport { rrResolution = resolutionNumber, rrAmount = 0, rrTime = 0}

maxResolution :: [Commitment] -> D.Natural
maxResolution commitments = maximum $ concat $ map commitmentResolutions commitments

resolutionReport :: [DayPlan] -> [ResolutionReport]
resolutionReport plans = 
  let schedules = concat $ map planSchedule plans
   in totalCommitment schedules <$> [0..maxResolution schedules]

