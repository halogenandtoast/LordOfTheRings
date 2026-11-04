module LordOfTheRings.Event.Cards.CaptainsOfTheWest (captainsOfTheWest) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Trait (Trait (Noble))

newtype CaptainsOfTheWest = CaptainsOfTheWest EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Quest Action.
captainsOfTheWest :: EventCard CaptainsOfTheWest
captainsOfTheWest = eventWith CaptainsOfTheWest Cards.captainsOfTheWest (criteriaL .~ DuringPhase QuestPhase)

-- | Until the end of the phase, each Noble hero gets +1 willpower and does not exhaust to
-- commit to the quest.
instance RunMessage CaptainsOfTheWest where
  runMessage msg e@(CaptainsOfTheWest attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      nobles <- select (HeroWithTrait Noble)
      for_ nobles \hid -> phaseModifiers attrs hid [WillpowerModifier 1, DoesNotExhaustToCommit]
      pure e
    _ -> CaptainsOfTheWest <$> runMessage msg attrs
