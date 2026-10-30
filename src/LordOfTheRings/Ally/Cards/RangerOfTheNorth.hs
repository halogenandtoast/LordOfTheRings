module LordOfTheRings.Ally.Cards.RangerOfTheNorth (rangerOfTheNorth) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype RangerOfTheNorth = RangerOfTheNorth AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter. Surge. Ranged. Sentinel, on its card def: revealed from the encounter
-- deck, it enters play under the first player (see the game's reveal).
rangerOfTheNorth :: AllyCard RangerOfTheNorth
rangerOfTheNorth = ally RangerOfTheNorth Cards.rangerOfTheNorth

-- | When Revealed: The first player chooses a player to take control of Ranger of the
-- North. Then, either deal 2 damage to an enemy or place 2 progress tokens on a location.
instance RunMessage RangerOfTheNorth where
  runMessage msg s@(RangerOfTheNorth a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      lead <- getLead
      choosePlayerM lead a \pid -> when (pid /= lead) $ takeControl a.id pid
      enemies <- select AnyEnemy
      locations <- select AnyLocation
      chooseOneM lead do
        questionLabeled "rangerOfTheNorth.damageOrProgress"
        causedBy a
        targets enemies \e -> dealDamage a e 2
        targets locations \l -> placeProgress a l 2
      pure s
    _ -> RangerOfTheNorth <$> runMessage msg a
