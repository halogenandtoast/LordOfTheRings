module LordOfTheRings.Enemy.Cards.InvadingOrc (invadingOrc) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers (returnALocation)

newtype InvadingOrc = InvadingOrc EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

invadingOrc :: EnemyCard InvadingOrc
invadingOrc = enemy InvadingOrc Cards.invadingOrc

-- | Forced: After Invading Orc attacks and destroys a character, that character's
-- controller returns a location he controls to the staging area.
instance HasAbilities InvadingOrc where
  getAbilities (InvadingOrc a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: If this attack destroys a character, return a location you control to the
-- staging area.
instance RunMessage InvadingOrc where
  runMessage msg e@(InvadingOrc attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      controllers <- mapMaybe (.controller) <$> attackDestroyed attrs.id
      for_ controllers (returnALocation attrs)
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      returnALocation attrs pid
      pure e
    _ -> InvadingOrc <$> runMessage msg attrs
