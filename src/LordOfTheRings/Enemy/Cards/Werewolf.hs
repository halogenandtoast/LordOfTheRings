module LordOfTheRings.Enemy.Cards.Werewolf (werewolf) where

import LordOfTheRings.Card (cdShadowEffect, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Werewolf = Werewolf EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

werewolf :: EnemyCard Werewolf
werewolf = enemy Werewolf Cards.werewolf

-- | Werewolf gets +1 attack for each damage on it.
instance HasModifiersFor Werewolf where
  getModifiersFor (Werewolf a) = when (a.damage > 0) $ modifySelf a [AttackModifier a.damage]

{- | Forced: After Werewolf is dealt a shadow card with no shadow effect, deal 1 damage to it
and the defending character. Read as each shadow card turns faceup ('revealedShadowCards'),
once for each such card.
Shadow: Deal 1 damage to the defending character.
-}
instance RunMessage Werewolf where
  runMessage msg e@(Werewolf attrs) = case msg of
    CheckWindows ws -> do
      for_ [defenders | (c, _, defenders) <- revealedShadowCards attrs ws, not (cdShadowEffect $ toCardDef c)] \defenders -> do
        dealDamage attrs attrs 1
        for_ defenders \d -> dealDamage attrs d 1
      Werewolf <$> runMessage msg attrs
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> dealDamage (CardIdSource cid) d 1
      pure e
    _ -> Werewolf <$> runMessage msg attrs
