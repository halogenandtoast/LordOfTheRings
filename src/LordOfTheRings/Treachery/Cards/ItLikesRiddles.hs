module LordOfTheRings.Treachery.Cards.ItLikesRiddles (itLikesRiddles) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim.Helpers (answerRiddle, hasRiddle)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ItLikesRiddles = ItLikesRiddles TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

itLikesRiddles :: TreacheryCard ItLikesRiddles
itLikesRiddles = treachery ItLikesRiddles Cards.itLikesRiddles

{- | When Revealed: The first player must choose to answer the riddle on this card. If he
finds at least 1 match, discard cards from the encounter deck until another card with a
riddle is discarded. Then, answer that riddle.

That riddle is the discarded card's own: answered for it, not for this card, so it does
not lead on to another.
-}
instance RunMessage ItLikesRiddles where
  runMessage msg t@(ItLikesRiddles attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      answerRiddle attrs False (MkEncounterCard attrs.cardId (toCardCode attrs))
      pure t
    ScenarioSpecific "riddle.answered" (parseMaybe parseJSON -> Just (source :: Source, matches :: Int))
      | isSource attrs source && matches > 0 -> do
          deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
          discardTopOfEncounterDeck attrs $ maybe (length deck) (+ 1) (findIndex hasRiddle deck)
          pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (lastMay $ filter hasRiddle cards) \c -> answerRiddle (CardIdSource c.id) False c
      pure t
    _ -> ItLikesRiddles <$> runMessage msg attrs
