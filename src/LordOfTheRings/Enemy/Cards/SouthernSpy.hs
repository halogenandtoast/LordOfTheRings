module LordOfTheRings.Enemy.Cards.SouthernSpy (southernSpy) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Fellowship (theOneRingExhausted)
import LordOfTheRings.Scenario.Scenarios.AKnifeInTheDark.Helpers (eitherRingwraithOr)

newtype SouthernSpy = SouthernSpy EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

southernSpy :: EnemyCard SouthernSpy
southernSpy = enemy SouthernSpy Cards.southernSpy

instance RunMessage SouthernSpy where
  runMessage msg e@(SouthernSpy a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      you <- getRevealer
      eitherRingwraithOr you a "southernSpy.choose" True "southernSpy.attack" $ immediateAttack you a.id
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      exhausted <- theOneRingExhausted
      for_ defenders \d -> attackModifier (CardIdSource cid) d (DefenseModifier $ if exhausted then -3 else -1)
      pure e
    _ -> SouthernSpy <$> runMessage msg a
