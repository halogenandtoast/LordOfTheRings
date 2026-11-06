module LordOfTheRings.Ally.Cards.HobbitArcher (hobbitArcher) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype HobbitArcher = HobbitArcher AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

hobbitArcher :: AllyCard HobbitArcher
hobbitArcher = ally HobbitArcher Cards.hobbitArcher

-- | Hobbit Archer gets +1 attack while attacking an enemy with a higher engagement cost
-- than your threat.
instance HasModifiersFor HobbitArcher where
  getModifiersFor (HobbitArcher a) =
    modifySelf a [AttackBonusAgainstMatching (EnemyWithEngagementCostAboveThreatOf a.controller) 1]
