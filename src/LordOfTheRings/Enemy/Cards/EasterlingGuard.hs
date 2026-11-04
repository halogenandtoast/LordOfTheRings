module LordOfTheRings.Enemy.Cards.EasterlingGuard (easterlingGuard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype EasterlingGuard = EasterlingGuard EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Archery 2, as its card def says.
easterlingGuard :: EnemyCard EasterlingGuard
easterlingGuard = enemy EasterlingGuard Cards.easterlingGuard

{- | When Revealed: Assign X damage among characters in play. X is the current archery
total: the first player assigns it, as archery damage is, its own archery counted.
Shadow: Defending character cannot ready this round.
-}
instance RunMessage EasterlingGuard where
  runMessage msg e@(EasterlingGuard attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- getArcheryTotal
      lead <- getLead
      characters <- selectCharacters AnyHero AnyAlly
      dealDamageDividedAmong attrs lead characters x
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> roundModifier (CardIdSource cid) d CannotReady
      pure e
    _ -> EasterlingGuard <$> runMessage msg attrs
