module LordOfTheRings.Enemy.Cards.UrukHaiFighter (urukHaiFighter) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype UrukHaiFighter = UrukHaiFighter EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Toughness 2, as its card def says.
urukHaiFighter :: EnemyCard UrukHaiFighter
urukHaiFighter = enemy UrukHaiFighter Cards.urukHaiFighter

-- | Forced: After Uruk-hai Fighter attacks and destroys a character, return it to the
-- staging area.
instance HasAbilities UrukHaiFighter where
  getAbilities (UrukHaiFighter a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: Discard a non-objective attachment you control.
instance RunMessage UrukHaiFighter where
  runMessage msg e@(UrukHaiFighter attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      returnToStagingAfterAttack attrs.id
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      discardNonObjectiveAttachment attrs pid
      pure e
    _ -> UrukHaiFighter <$> runMessage msg attrs
