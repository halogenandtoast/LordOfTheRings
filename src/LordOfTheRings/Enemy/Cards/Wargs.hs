module LordOfTheRings.Enemy.Cards.Wargs (wargs) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Wargs = Wargs EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

wargs :: EnemyCard Wargs
wargs = enemy Wargs Cards.wargs

{- | Forced: If Wargs is dealt a shadow card with no effect, return Wargs to the staging
area after it attacks.
Read as each shadow card turns faceup ('revealedShadowCards'), as Nate French rules on this
card (ALeP 3.003): one discarded before then does not count, one discarded after it still
does, and Wargs returns once the attack is over.
Shadow: attacking enemy gets +1 attack. (+2 attack instead if this attack is undefended.)
-}
instance RunMessage Wargs where
  runMessage msg e@(Wargs attrs) = case msg of
    CheckWindows ws -> do
      when (any (not . cdShadowEffect . toCardDef) [c | (c, _, _) <- revealedShadowCards attrs ws])
        $ returnToStagingAfterAttack attrs.id
      Wargs <$> runMessage msg attrs
    ResolveShadowEffect cid eid _ defender | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid (ifUndefended defender 2 1)
      pure e
    _ -> Wargs <$> runMessage msg attrs
