module LordOfTheRings.Event.Cards.TheWizardsVoice (theWizardsVoice) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype TheWizardsVoice = TheWizardsVoice EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Doomed 3, on its card def. Action: Each player chooses 1 enemy engaged with him. Until
-- the end of the phase, each chosen enemy cannot attack the player that chose it.
theWizardsVoice :: EventCard TheWizardsVoice
theWizardsVoice = event TheWizardsVoice Cards.theWizardsVoice

instance RunMessage TheWizardsVoice where
  runMessage msg e@(TheWizardsVoice attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      eachPlayer \p -> do
        enemies <- select (EnemyEngagedWith $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "chooseEnemy"
          causedBy attrs
          targets enemies \eid -> phaseModifier attrs eid (CannotAttackPlayer p)
      pure e
    _ -> TheWizardsVoice <$> runMessage msg attrs
