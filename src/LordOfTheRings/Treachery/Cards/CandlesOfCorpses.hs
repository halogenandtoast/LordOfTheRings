module LordOfTheRings.Treachery.Cards.CandlesOfCorpses (candlesOfCorpses) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CandlesOfCorpses = CandlesOfCorpses TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

candlesOfCorpses :: TreacheryCard CandlesOfCorpses
candlesOfCorpses = treachery CandlesOfCorpses Cards.candlesOfCorpses

-- | When Revealed: Each player makes an escape test, dealing 1 card from the encounter deck.
-- Each player who fails this test must either discard a hero he controls from play or
-- raise his threat by 5.
instance RunMessage CandlesOfCorpses where
  runMessage msg t@(CandlesOfCorpses attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        heroes <- select (HeroControlledBy $ PlayerWithId pid)
        escapeTest pid attrs 1 EscapeWillpower (failed pid heroes) (pure ())
      pure t
    _ -> CandlesOfCorpses <$> runMessage msg attrs
   where
    failed pid heroes = chooseOneM pid do
      questionLabeled "candlesOfCorpses.choose"
      causedBy attrs
      labeled "candlesOfCorpses.threat" $ encounterRaisesThreat pid 5
      targets heroes discard
