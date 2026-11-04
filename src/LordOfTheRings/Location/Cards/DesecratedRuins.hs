module LordOfTheRings.Location.Cards.DesecratedRuins (desecratedRuins) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers

newtype DesecratedRuins = DesecratedRuins LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

desecratedRuins :: LocationCard DesecratedRuins
desecratedRuins = location DesecratedRuins Cards.desecratedRuins

-- | Deep. While there are 3 or more quest cards in the victory display, Desecrated Ruins
-- gets +2 threat.
instance HasModifiersFor DesecratedRuins where
  getModifiersFor (DesecratedRuins a) = do
    quests <- questCardsInVictoryDisplay
    when (quests >= 3) $ modifySelf a [ThreatModifier 2]

-- | Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, each
-- player discards a random card from his hand.
instance RunMessage DesecratedRuins where
  runMessage msg l@(DesecratedRuins a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack a eid 1
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      eachPlayer \p -> discardRandomFromHand p 1
      pure l
    _ -> DesecratedRuins <$> runMessage msg a
