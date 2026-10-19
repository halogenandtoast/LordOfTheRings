module LordOfTheRings.Matcher.Window (module LordOfTheRings.Matcher.Window) where

import LordOfTheRings.Id
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Timing
import LordOfTheRings.Window

data WindowMatcher
  = EntersPlayWindow Timing Target
  | PhaseBeginsWindow Timing Phase
  | CommittedToQuestWindow Timing Target
  | EnemyEngagedWindow Timing EnemyId
  | DealtDamageWindow Timing Target
  | TraveledWindow Timing LocationId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

windowMatches :: WindowMatcher -> Window -> Bool
windowMatches matcher (Window timing wType) = case (matcher, wType) of
  (EntersPlayWindow t target, EntersPlay target') -> t == timing && target == target'
  (PhaseBeginsWindow t phase, PhaseBegins phase') -> t == timing && phase == phase'
  (CommittedToQuestWindow t target, CommittedToQuest target') -> t == timing && target == target'
  (EnemyEngagedWindow t eid, EnemyEngaged eid' _) -> t == timing && eid == eid'
  (DealtDamageWindow t target, DealtDamage target' _) -> t == timing && target == target'
  (TraveledWindow t lid, Traveled lid') -> t == timing && lid == lid'
  _ -> False
