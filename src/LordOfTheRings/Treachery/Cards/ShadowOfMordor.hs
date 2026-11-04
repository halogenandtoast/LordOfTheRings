module LordOfTheRings.Treachery.Cards.ShadowOfMordor (shadowOfMordor) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ShadowOfMordor = ShadowOfMordor TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 1.
shadowOfMordor :: TreacheryCard ShadowOfMordor
shadowOfMordor = treachery ShadowOfMordor Cards.shadowOfMordor

{- | When Revealed: Each player deals 1 damage to a hero he controls. Until the end of the
round, treat the printed text box of each damaged character as if it were blank, except
for Traits. Each character this effect damaged is blanked, once all of it is dealt.
-}
instance RunMessage ShadowOfMordor where
  runMessage msg t@(ShadowOfMordor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> dealDamageToOneHero attrs p 1
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      damaged <- damageDealtBy (toSource attrs)
      for_ (ordNub [c | (c, n) <- damaged, n > 0]) \c -> roundModifier attrs c Blank
      pure t
    _ -> ShadowOfMordor <$> runMessage msg attrs
