module LordOfTheRings.Event.Cards.WaitNoLonger (waitNoLonger) where

import LordOfTheRings.Card (Card (EncounterCard), cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype WaitNoLonger = WaitNoLonger EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: At the beginning of the quest phase, search the top 5 cards of the encounter
-- deck for an enemy and put it into play engaged with you. Then, reveal one less
-- encounter card this phase (to a minimum of 0). Shuffle the encounter deck.
waitNoLonger :: EventCard WaitNoLonger
waitNoLonger = eventWith WaitNoLonger Cards.waitNoLonger (windowL ?~ PhaseBeginsWindow After QuestPhase)

instance RunMessage WaitNoLonger where
  runMessage msg e@(WaitNoLonger attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      top <- getsGame (take 5 . (.encounterDeck) . toAttrs . (.scenario))
      let enemies = filter ((== EnemyType) . cdCardType . toCardDef) top
      push
        $ Ask pid
        $ QuestionLabel (ikey "label.waitNoLonger.chooseEnemy") (Just $ toTarget attrs) []
        $ SearchCards (map EncounterCard top)
        $ ChooseOne
        $ if null enemies
          then [Label (ikey "label.continue") []]
          else [targetLabel c [RemoveFromEncounterDeck c, PutEnemyIntoPlayEngaged pid c] | c <- enemies]
      phaseModifier attrs GameTarget (AdditionalStagingReveals (-1))
      push $ ShuffleDeck EncounterDeck
      pure e
    _ -> WaitNoLonger <$> runMessage msg attrs
