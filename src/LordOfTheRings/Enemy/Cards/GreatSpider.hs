module LordOfTheRings.Enemy.Cards.GreatSpider (greatSpider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GreatSpider = GreatSpider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

greatSpider :: EnemyCard GreatSpider
greatSpider = enemy GreatSpider Cards.greatSpider

instance HasAbilities GreatSpider where
  getAbilities (GreatSpider a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage GreatSpider where
  runMessage msg e@(GreatSpider a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> chooseAndExhaust a pid 1
      pure e
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      characters <- exhaustedCharactersOf pid
      chooseOrRunOneM pid do
        questionLabeled "greatSpider.damage"
        causedBy a
        targets characters \c -> dealDamage a c 1
      pure e
    _ -> GreatSpider <$> runMessage msg a
