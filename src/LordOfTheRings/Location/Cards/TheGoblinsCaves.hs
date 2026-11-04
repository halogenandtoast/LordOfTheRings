module LordOfTheRings.Location.Cards.TheGoblinsCaves (theGoblinsCaves) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Trait (Trait (Goblin))

newtype TheGoblinsCaves = TheGoblinsCaves LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theGoblinsCaves :: LocationCard TheGoblinsCaves
theGoblinsCaves = location TheGoblinsCaves Cards.theGoblinsCaves

-- | While The Goblins' Caves is the active location, Goblin enemies get +1 threat.
instance HasModifiersFor TheGoblinsCaves where
  getModifiersFor (TheGoblinsCaves a) = when (a.placement == ActiveLocation) do
    goblins <- select (EnemyWithTrait Goblin)
    for_ goblins \g -> modified a g [ThreatModifier 1]

-- | Shadow: Defending player raises his threat by X. X is the number of Goblin enemies
-- engaged with him.
instance RunMessage TheGoblinsCaves where
  runMessage msg l@(TheGoblinsCaves a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      goblins <- length <$> select (EnemyWithTrait Goblin <> EnemyEngagedWith (PlayerWithId pid))
      encounterRaisesThreat pid goblins
      pure l
    _ -> TheGoblinsCaves <$> runMessage msg a
