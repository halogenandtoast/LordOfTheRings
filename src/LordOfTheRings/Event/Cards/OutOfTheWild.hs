module LordOfTheRings.Event.Cards.OutOfTheWild (outOfTheWild) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Card (Card (EncounterCard), cdCardType, cdVictoryPoints, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Scenario.Types (scenarioEncounterDeck)

newtype OutOfTheWild = OutOfTheWild EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Secrecy 2, on its card def.
-- Action: Search the top 5 cards of the encounter deck for any 1 non-objective card worth
-- no victory points and add it to your victory display. Shuffle the encounter deck. Add
-- Out of the Wild to the victory display.
outOfTheWild :: EventCard OutOfTheWild
outOfTheWild = event OutOfTheWild Cards.outOfTheWild

instance RunMessage OutOfTheWild where
  runMessage msg e@(OutOfTheWild attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      top <- getsGame (take 5 . scenarioEncounterDeck . toAttrs . gameScenario)
      let eligible c =
            cdCardType (toCardDef c) `notElem` [ObjectiveType, ObjectiveAllyType]
              && maybe True (== 0) (cdVictoryPoints $ toCardDef c)
      chooseOneM pid do
        questionLabeled "outOfTheWild.choose"
        causedBy attrs
        showing (map EncounterCard top)
        targets (filter eligible top) addToVictoryDisplay
        labeled "outOfTheWild.none" (pure ())
      shuffleEncounterDeck
      addToVictoryDisplay card
      pure e
    _ -> OutOfTheWild <$> runMessage msg attrs
