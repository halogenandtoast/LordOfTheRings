module LordOfTheRings.Location.Cards.TheCursedTomb (theCursedTomb) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (lootControlled)

newtype TheCursedTomb = TheCursedTomb LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

theCursedTomb :: LocationCard TheCursedTomb
theCursedTomb = location TheCursedTomb Cards.theCursedTomb

-- | Immune to player card effects. The players cannot travel to The Cursed Tomb unless they
-- control 3 Loot objectives.
instance HasModifiersFor TheCursedTomb where
  getModifiersFor (TheCursedTomb a) = do
    loot <- lootControlled
    modifySelf a $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | loot < 3]

-- | Forced: After the players travel to The Cursed Tomb, search the encounter deck and
-- discard pile for Fram's Shade and add it to the staging area. Shuffle the encounter deck.
-- Fram's Shade makes an immediate attack against each player in turn order.
instance HasAbilities TheCursedTomb where
  getAbilities (TheCursedTomb a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

-- | Fram's Shade attacks wherever it is, should it already be in play.
instance RunMessage TheCursedTomb where
  runMessage msg l@(TheCursedTomb a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchEncounterDeckAndDiscard pid a (cardIs Enemies.framsShade)
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      shades <- select (EnemyWithCardCode $ toCardCode Enemies.framsShade)
      eachPlayer \p -> for_ shades (immediateAttack p)
      pure l
    _ -> TheCursedTomb <$> runMessage msg a
