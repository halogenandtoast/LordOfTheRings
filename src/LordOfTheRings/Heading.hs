-- | The Dream-chaser's heading (Grey Havens rules): one card of four settings, from the
-- best, on-course, to the worst.
module LordOfTheRings.Heading (module LordOfTheRings.Heading) where

import LordOfTheRings.Prelude

-- | Sunny is on-course; Cloudy, Rainy and Stormy are off-course, Stormy the worst.
data Heading = Sunny | Cloudy | Rainy | Stormy
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Which way the heading shifts: one setting, never past Sunny or Stormy.
data HeadingShift = ShiftOnCourse | ShiftOffCourse
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

shiftHeading :: HeadingShift -> Heading -> Heading
shiftHeading = \case
  ShiftOnCourse -> \h -> if h == minBound then h else pred h
  ShiftOffCourse -> \h -> if h == maxBound then h else succ h

-- | What a card reads of the heading: on-course, off-course, or at the worst setting.
data HeadingMatcher = OnCourseHeading | OffCourseHeading | WorstHeading
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

headingIs :: HeadingMatcher -> Heading -> Bool
headingIs = \case
  OnCourseHeading -> (== Sunny)
  OffCourseHeading -> (/= Sunny)
  WorstHeading -> (== Stormy)
