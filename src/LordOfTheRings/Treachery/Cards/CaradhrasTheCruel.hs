module LordOfTheRings.Treachery.Cards.CaradhrasTheCruel (caradhrasTheCruel) where

import LordOfTheRings.Trait (Trait (Snow))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CaradhrasTheCruel = CaradhrasTheCruel TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

caradhrasTheCruel :: TreacheryCard CaradhrasTheCruel
caradhrasTheCruel = treachery CaradhrasTheCruel Cards.caradhrasTheCruel

-- | When Revealed: Return all Snow cards in the encounter discard pile to the top of the
-- encounter deck, in random order. Reveal the top card of the encounter deck (reveal the
-- top 2 cards instead if there are 3 or 4 players in the game).
instance RunMessage CaradhrasTheCruel where
  runMessage msg t@(CaradhrasTheCruel attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      snow <- shuffleM . filter (cardMatches $ CardWithTrait Snow) =<< getEncounterDiscard
      traverse_ moveToTopOfEncounterDeck snow
      players <- length <$> getRemainingPlayers
      revealEncounterCards (if players >= 3 then 2 else 1)
      pure t
    _ -> CaradhrasTheCruel <$> runMessage msg attrs
