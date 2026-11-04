module LordOfTheRings.Location.Cards.TheLonelyMountain (theLonelyMountain) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheLonelyMountain = TheLonelyMountain LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | Stage 1A stacks the five Erebor treasures facedown under it.

Action: While making a burgle attempt, the first player may spend 2 Baggins resources to
take the revealed card into hand and reveal the next card of his deck. The attempt offers
it as the first player decides whether to discard (see
"LordOfTheRings.Scenario.Scenarios.TheLonelyMountain"), the only time it may be taken.
-}
theLonelyMountain :: LocationCard TheLonelyMountain
theLonelyMountain = location TheLonelyMountain Cards.theLonelyMountain

-- | While at least 1 card is stacked underneath, The Lonely Mountain is immune to player
-- card effects, cannot leave the staging area, and gets -2 threat for each card stacked
-- under it. Not leaving it, it cannot be traveled to.
instance HasModifiersFor TheLonelyMountain where
  getModifiersFor (TheLonelyMountain a) = do
    let stacked = length a.underneath
    when (stacked > 0) $ modifySelf a [ImmuneToPlayerCardEffects, CannotLeaveStagingArea, CannotBeTraveledTo, ThreatModifier (-2 * stacked)]
