module LordOfTheRings.Enemy.Cards.WickedSpider (wickedSpider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (givePoisonToCharacterOf)

newtype WickedSpider = WickedSpider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Venom, on its card def.
wickedSpider :: EnemyCard WickedSpider
wickedSpider = enemy WickedSpider Cards.wickedSpider

-- | Forced: When this enemy attacks, defending player must attach 1 poison to a character
-- he controls.
instance HasAbilities WickedSpider where
  getAbilities (WickedSpider a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Shadow: Defending character gets 2 poison.
instance RunMessage WickedSpider where
  runMessage msg e@(WickedSpider attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid -> givePoisonToCharacterOf attrs pid 1
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> givePoison d 2
      pure e
    _ -> WickedSpider <$> runMessage msg attrs
