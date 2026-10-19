module LordOfTheRings.Enemy.Cards.ForestSpider (forestSpider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype ForestSpider = ForestSpider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

forestSpider :: EnemyCard ForestSpider
forestSpider = enemy ForestSpider Cards.forestSpider

instance HasAbilities ForestSpider where
  getAbilities (ForestSpider a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage ForestSpider where
  runMessage msg e@(ForestSpider attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      push $ CreateEffect $ Effect (toSource attrs) (toTarget attrs) [AttackModifier 1] UntilEndOfRound
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      push . ChooseAndDiscard (EnemySource eid) pid =<< getAttachmentsControlledBy pid
      pure e
    _ -> ForestSpider <$> runMessage msg attrs
