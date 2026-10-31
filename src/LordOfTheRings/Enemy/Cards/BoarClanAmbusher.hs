module LordOfTheRings.Enemy.Cards.BoarClanAmbusher (boarClanAmbusher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype BoarClanAmbusher = BoarClanAmbusher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

boarClanAmbusher :: EnemyCard BoarClanAmbusher
boarClanAmbusher = enemy BoarClanAmbusher Cards.boarClanAmbusher

-- | Forced: After Boar Clan Ambusher attacks and destroys a character, discard a random
-- card from your hand.
instance HasAbilities BoarClanAmbusher where
  getAbilities (BoarClanAmbusher a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: If this attack destroys a character, discard a random card from your hand.
instance RunMessage BoarClanAmbusher where
  runMessage msg e@(BoarClanAmbusher attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (CharacterDestroyedByAttack _ _ pid) <- ws] \pid -> discardRandomFromHand pid 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      discardRandomFromHand pid 1
      pure e
    _ -> BoarClanAmbusher <$> runMessage msg attrs
