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
Read as its shadow cards resolve, as Mountain Warg's: the additional attack resolves no
shadow card, the one dealt for this attack already turned faceup.
Shadow: Return attacking enemy to the staging area after this attack.
-}
instance RunMessage PackOfWargs where
  runMessage msg e@(PackOfWargs attrs) = case msg of
    ResolveShadow eid pid _ | eid == attrs.id -> do
      let faceDown = filter ((`notElem` attrs.enemyShadowRevealed) . (.id)) attrs.shadowCards
      when (any (not . cdShadowEffect . toCardDef) faceDown) do
        again <- capture $ immediateAttackWithoutShadow pid eid
        afterAttack eid again
      PackOfWargs <$> runMessage msg attrs
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure e
    _ -> PackOfWargs <$> runMessage msg attrs
