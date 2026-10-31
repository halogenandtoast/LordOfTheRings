module LordOfTheRings.Enemy.Cards.OrcAbductor (orcAbductor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Quests

newtype OrcAbductor = OrcAbductor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcAbductor :: EnemyCard OrcAbductor
orcAbductor = enemy OrcAbductor Cards.orcAbductor

-- | Forced: At the end of the round, if Orc Abductor is in the staging area, search the
-- encounter deck, discard pile and victory display for Rescue Iârion, and reveal it.
-- Shuffle the encounter deck.
instance HasAbilities OrcAbductor where
  getAbilities (OrcAbductor a) = [mkAbility a 1 $ forced RoundEndsWindow | a.placement == StagingArea]

-- | Shadow: Deal 1 damage to the defending character.
instance RunMessage OrcAbductor where
  runMessage msg e@(OrcAbductor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      searchAndRevealEncounterCard (cardIs Quests.rescueIarion)
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage attrs d 1
      pure e
    _ -> OrcAbductor <$> runMessage msg attrs
