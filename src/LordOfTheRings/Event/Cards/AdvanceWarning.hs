module LordOfTheRings.Event.Cards.AdvanceWarning (advanceWarning) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))

newtype AdvanceWarning = AdvanceWarning EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if each hero you control has a printed lore resource icon.
-- Action: Until the end of the phase, enemies do not make engagement checks: those in play
-- as it is played, a lasting effect of a player card being worked out once (FAQ 1.55).
advanceWarning :: EventCard AdvanceWarning
advanceWarning = eventWith AdvanceWarning Cards.advanceWarning \a -> a & criteriaL .~ eachHeroHasPrintedIcon a.owner Lore

instance RunMessage AdvanceWarning where
  runMessage msg e@(AdvanceWarning attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      enemies <- select AnyEnemy
      for_ enemies \eid -> phaseModifier attrs eid NoEngagementChecks
      pure e
    _ -> AdvanceWarning <$> runMessage msg attrs
