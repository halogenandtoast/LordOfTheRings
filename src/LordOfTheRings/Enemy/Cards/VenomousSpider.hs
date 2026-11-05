module LordOfTheRings.Enemy.Cards.VenomousSpider (venomousSpider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype VenomousSpider = VenomousSpider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

venomousSpider :: EnemyCard VenomousSpider
venomousSpider = enemy VenomousSpider Cards.venomousSpider

instance HasAbilities VenomousSpider where
  getAbilities (VenomousSpider a) = [mkAbility a 1 $ forced $ DefenderDeclaredAgainstWindow After a.id]

instance RunMessage VenomousSpider where
  runMessage msg e@(VenomousSpider a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [d | Window _ (DeclaredAsDefender d _) <- ws] \d -> givePoison d 1
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders \d -> givePoison d 2
      pure e
    _ -> VenomousSpider <$> runMessage msg a
