module LordOfTheRings.Event.Cards.FarSighted (farSighted) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Scenario.Types (scenarioEncounterDeck)

newtype FarSighted = FarSighted EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Doomed 1, on its card def. Action: Look at the top 5 cards of the encounter deck. Put
-- them back in the same order.
farSighted :: EventCard FarSighted
farSighted = event FarSighted Cards.farSighted

instance RunMessage FarSighted where
  runMessage msg e@(FarSighted attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      top <- getsGame (take 5 . scenarioEncounterDeck . toAttrs . gameScenario)
      unless (null top) $ chooseOneM pid do
        questionLabeled "farSighted.look"
        causedBy attrs
        showing (map EncounterCard top)
        labeled "farSighted.done" $ pure ()
      pure e
    _ -> FarSighted <$> runMessage msg attrs
