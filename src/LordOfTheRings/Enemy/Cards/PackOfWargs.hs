module LordOfTheRings.Enemy.Cards.PackOfWargs (packOfWargs) where

import LordOfTheRings.Card (cdShadowEffect, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype PackOfWargs = PackOfWargs EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

packOfWargs :: EnemyCard PackOfWargs
packOfWargs = enemy PackOfWargs Cards.packOfWargs

{- | Forced: If Pack of Wargs is dealt a shadow card with no effect, Pack of Wargs makes an
additional attack after this one. (Do not deal it a shadow card.)
Read as each shadow card turns faceup ('revealedShadowCards'): the additional attack
resolves no shadow card, the one dealt for this attack already turned faceup.
Shadow: Return attacking enemy to the staging area after this attack.
-}
instance RunMessage PackOfWargs where
  runMessage msg e@(PackOfWargs attrs) = case msg of
    CheckWindows ws -> do
      for_ [pid | (c, pid, _) <- revealedShadowCards attrs ws, not (cdShadowEffect $ toCardDef c)] \pid -> do
        again <- capture $ immediateAttackWithoutShadow pid attrs.id
        afterAttack attrs.id again
      PackOfWargs <$> runMessage msg attrs
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure e
    _ -> PackOfWargs <$> runMessage msg attrs
