module LordOfTheRings.Event.Cards.Interrogation (interrogation) where

import LordOfTheRings.Card (Card (EncounterCard), cdThreat, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Scenario.Types (scenarioEncounterDeck)
import LordOfTheRings.Trait (Trait (Trap))

newtype Interrogation = Interrogation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

interrogation :: EventCard Interrogation
interrogation = eventWith Interrogation Cards.interrogation (criteriaL .~ exists (EnemyWithAttached Trap))

-- | Action: Choose an enemy with an attached Trap card. Look at the top X cards of the
-- encounter deck, where X is that enemy's printed threat. You may discard 1 of those
-- cards. Return the rest to the top of the deck, in the same order.
instance RunMessage Interrogation where
  runMessage msg e@(Interrogation attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select (EnemyWithAttached Trap)
      chooseOrRunOneM pid do
        questionLabeled "interrogation.enemy"
        causedBy attrs
        targets enemies \eid -> do
          x <- fromMaybe 0 . cdThreat . toCardDef <$> getEnemy eid
          top <- getsGame (take x . scenarioEncounterDeck . toAttrs . gameScenario)
          unless (null top) $ chooseOneM pid do
            questionLabeled "interrogation.discard"
            causedBy attrs
            showing (map EncounterCard top)
            targets top discardFromEncounterDeck
            labeled "interrogation.keep" $ pure ()
      pure e
    _ -> Interrogation <$> runMessage msg attrs
