module LordOfTheRings.Enemy.Cards.ForestFlies (forestFlies) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Trait (Trait (Insect))

newtype ForestFlies = ForestFlies EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

forestFlies :: EnemyCard ForestFlies
forestFlies = enemy ForestFlies Cards.forestFlies

instance HasAbilities ForestFlies where
  getAbilities (ForestFlies a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Forced: After you engage Forest Flies, deal 1 damage to each exhausted character you
-- control.
-- Shadow: If attacking enemy is an Insect, deal it 2 additional shadow cards.
instance RunMessage ForestFlies where
  runMessage msg e@(ForestFlies attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [p | EngagedWith p <- [attrs.placement]] \pid -> do
        exhausted <- exhaustedCharactersOf pid
        for_ exhausted \c -> dealDamage attrs c 1
      pure e
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      insect <- selectAny (EnemyWithId eid <> EnemyWithTrait Insect)
      when insect $ dealShadowCardsDuringAttack eid pid defenders 2
      pure e
    _ -> ForestFlies <$> runMessage msg attrs
