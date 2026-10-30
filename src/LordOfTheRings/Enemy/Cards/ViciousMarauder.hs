module LordOfTheRings.Enemy.Cards.ViciousMarauder (viciousMarauder) where

import LordOfTheRings.Card (cdCost, costAsResources, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Player.Types (discardableHand)

newtype ViciousMarauder = ViciousMarauder EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

viciousMarauder :: EnemyCard ViciousMarauder
viciousMarauder = enemy ViciousMarauder Cards.viciousMarauder

-- | Forced: When Vicious Marauder engages you, discard a random card from your hand and
-- place resources on it equal to that card's printed cost.
-- Forced: When Vicious Marauder attacks you, distribute damage among characters you
-- control equal to the number of resources on it.
instance HasAbilities ViciousMarauder where
  getAbilities (ViciousMarauder a) =
    [ mkAbility a 1 $ forced $ EnemyEngagedWindow When a.id
    , mkAbility a 2 $ forced $ EnemyAttacksWindow When a.id
    ]

instance RunMessage ViciousMarauder where
  runMessage msg e@(ViciousMarauder attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        hand <- discardableHand <$> getPlayer pid
        unless (null hand) do
          card <- uniform hand
          discardFromHand pid card
          placeResources attrs attrs $ maybe 0 costAsResources (cdCost $ toCardDef card)
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 ws -> do
      let n = countTokens Resource attrs.enemyTokens
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid -> when (n > 0) $ dealDamageDivided attrs pid n
      pure e
    _ -> ViciousMarauder <$> runMessage msg attrs
