module LordOfTheRings.Event.Cards.RiskSomeLight (riskSomeLight) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Card (Card (EncounterCard), EncounterCard)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Scenario.Types (scenarioEncounterDeck)

newtype RiskSomeLight = RiskSomeLight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Secrecy 3, on its card def.
-- Action: Look at the top 3 cards of the encounter deck. You may select 1 card and move
-- it to the bottom of the encounter deck. Return any unselected cards to the top of the
-- encounter deck, in any order.
riskSomeLight :: EventCard RiskSomeLight
riskSomeLight = event RiskSomeLight Cards.riskSomeLight

instance RunMessage RiskSomeLight where
  runMessage msg e@(RiskSomeLight attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      top <- getsGame (take 3 . scenarioEncounterDeck . toAttrs . gameScenario)
      -- each goes on top as it is chosen, so the last one chosen ends on top
      let reorder :: ReverseQueue n => [EncounterCard] -> n ()
          reorder cards = chooseOneAtATimeM pid do
            questionLabeled "riskSomeLight.order"
            causedBy attrs
            showing (map EncounterCard cards)
            targets cards \c -> do
              removeFromEncounterDeck c
              push $ PutOnTopOfEncounterDeck c
      chooseOneM pid do
        questionLabeled "riskSomeLight.choose"
        causedBy attrs
        showing (map EncounterCard top)
        targets top \c -> do
          removeFromEncounterDeck c
          push $ PutOnBottomOfEncounterDeck [c]
          reorder (filter (/= c) top)
        labeled "riskSomeLight.none" $ reorder top
      pure e
    _ -> RiskSomeLight <$> runMessage msg attrs
