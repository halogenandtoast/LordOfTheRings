module LordOfTheRings.Treachery.Cards.SpiesFromMordor (spiesFromMordor) where

import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (cardsUnderOrcsSearch)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SpiesFromMordor = SpiesFromMordor TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

spiesFromMordor :: TreacheryCard SpiesFromMordor
spiesFromMordor = treachery SpiesFromMordor Cards.spiesFromMordor

-- | When Revealed: Each player must exhaust X characters he controls. X is the number of
-- cards underneath The Orcs' Search. If no characters are exhausted this way, Spies from
-- Mordor gains Surge.
-- Shadow: Defending player discards a non-objective attachment he controls.
instance RunMessage SpiesFromMordor where
  runMessage msg t@(SpiesFromMordor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- cardsUnderOrcsSearch
      readied <- selectCharacters ReadyHero ReadyAlly
      if x > 0 && notNull readied
        then eachPlayer \pid -> chooseAndExhaust attrs pid x
        else revealEncounterCard
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      attachments <- filter (\case ObjectiveTarget _ -> False; _ -> True) <$> getAttachmentsControlledBy pid
      chooseOrRunOneM pid do
        questionLabeled "spiesFromMordor.discard"
        causedBy (CardIdTarget cid)
        targets attachments discard
      pure t
    _ -> SpiesFromMordor <$> runMessage msg attrs
