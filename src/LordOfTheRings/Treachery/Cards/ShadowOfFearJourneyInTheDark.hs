module LordOfTheRings.Treachery.Cards.ShadowOfFearJourneyInTheDark (shadowOfFearJourneyInTheDark) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ShadowOfFearJourneyInTheDark = ShadowOfFearJourneyInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of Journey in the Dark's burden set. Doomed 1. Surge.
shadowOfFearJourneyInTheDark :: TreacheryCard ShadowOfFearJourneyInTheDark
shadowOfFearJourneyInTheDark = treachery ShadowOfFearJourneyInTheDark Cards.shadowOfFearJourneyInTheDark

{- | When Revealed: Treat the printed text box of each character in play as blank (except
for Traits) until the end of the round: each one in play as it resolves.
Shadow: Defending character does not count its defense.
-}
instance RunMessage ShadowOfFearJourneyInTheDark where
  runMessage msg t@(ShadowOfFearJourneyInTheDark attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> roundModifier attrs c Blank
      pure t
    ResolveShadowEffect cid _ _ defender | cid == attrs.cardId -> do
      for_ defender \d -> attackModifier attrs d DoesNotCountDefense
      pure t
    _ -> ShadowOfFearJourneyInTheDark <$> runMessage msg attrs
