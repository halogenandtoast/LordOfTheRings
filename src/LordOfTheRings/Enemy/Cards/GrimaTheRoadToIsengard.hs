module LordOfTheRings.Enemy.Cards.GrimaTheRoadToIsengard (grimaTheRoadToIsengard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GrimaTheRoadToIsengard = GrimaTheRoadToIsengard EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 2, as its card def says.
grimaTheRoadToIsengard :: EnemyCard GrimaTheRoadToIsengard
grimaTheRoadToIsengard = enemy GrimaTheRoadToIsengard Cards.grimaTheRoadToIsengard

-- | Forced: After Gríma attacks a player, that player shuffles Gríma back into the
-- encounter deck and reveals an encounter card.
instance HasAbilities GrimaTheRoadToIsengard where
  getAbilities (GrimaTheRoadToIsengard a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

-- | Shadow: This attack is considered undefended.
instance RunMessage GrimaTheRoadToIsengard where
  runMessage msg e@(GrimaTheRoadToIsengard attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (headMay [pid | Window After (EnemyAttacking eid pid) <- ws, eid == attrs.id]) \pid -> do
        shuffleIntoEncounterDeck attrs
        asRevealer pid revealEncounterCard
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      makeUndefended pid eid
      pure e
    _ -> GrimaTheRoadToIsengard <$> runMessage msg attrs
