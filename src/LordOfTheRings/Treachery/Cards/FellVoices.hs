module LordOfTheRings.Treachery.Cards.FellVoices (fellVoices) where

import LordOfTheRings.Card.CardType (CardType (TreacheryType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Trait (Trait (Snow))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FellVoices = FellVoices TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fellVoices :: TreacheryCard FellVoices
fellVoices = treachery FellVoices Cards.fellVoices

-- | When Revealed: Return the top 2 Snow cards in the encounter discard pile to the top of
-- the encounter deck. If this effect returned less than 2 Snow treachery cards, Fell
-- Voices gains surge.
instance RunMessage FellVoices where
  runMessage msg t@(FellVoices attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      pile <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      let snow = take 2 $ filter (cardMatches $ CardWithTrait Snow) pile
      -- the topmost goes back last, to stay on top
      for_ (reverse snow) moveToTopOfEncounterDeck
      when (length (filter (cardMatches $ CardWithType TreacheryType) snow) < 2) revealEncounterCard
      pure t
    _ -> FellVoices <$> runMessage msg attrs
