module LordOfTheRings.Event.Cards.ExpectingMischief (expectingMischief) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype ExpectingMischief = ExpectingMischief EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play during the quest phase, before the staging step.
-- Action: Deal 2 damage to the first enemy revealed from the encounter deck this phase.
--
-- The damage is dealt as the enemy is revealed, before its When Revealed effect and
-- keywords, which one it destroys does not resolve (FAQ 1.9, as Thalin's).
expectingMischief :: EventCard ExpectingMischief
expectingMischief =
  eventWith ExpectingMischief Cards.expectingMischief \a ->
    a & criteriaL .~ (DuringPhase QuestPhase <> BeforeStaging)

instance RunMessage ExpectingMischief where
  runMessage msg e@(ExpectingMischief attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      phaseModifier attrs GameTarget (DamageNextRevealedEnemy 2)
      pure e
    _ -> ExpectingMischief <$> runMessage msg attrs
