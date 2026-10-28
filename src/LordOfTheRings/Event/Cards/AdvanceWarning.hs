module LordOfTheRings.Event.Cards.AdvanceWarning (advanceWarning) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))

newtype AdvanceWarning = AdvanceWarning EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if each hero you control has a printed lore resource icon.
-- Action: Until the end of the phase, enemies do not make engagement checks.
advanceWarning :: EventCard AdvanceWarning
advanceWarning = eventWith AdvanceWarning Cards.advanceWarning \a -> a & criteriaL .~ eachHeroHasPrintedIcon a.owner Lore

instance RunMessage AdvanceWarning where
  runMessage msg e@(AdvanceWarning attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      phaseModifier attrs GameTarget NoEngagementChecks
      pure e
    _ -> AdvanceWarning <$> runMessage msg attrs
