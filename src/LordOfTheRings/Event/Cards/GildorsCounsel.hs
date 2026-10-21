module LordOfTheRings.Event.Cards.GildorsCounsel (gildorsCounsel) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype GildorsCounsel = GildorsCounsel EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play during the quest phase, before characters are committed to the quest.
-- Action: Reveal 1 less card from the encounter deck this phase. (To a minimum of 1.)
gildorsCounsel :: EventCard GildorsCounsel
gildorsCounsel = eventWith GildorsCounsel Cards.gildorsCounsel (criteriaL .~ BeforeCommitting)

instance RunMessage GildorsCounsel where
  runMessage msg e@(GildorsCounsel attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      phaseModifier attrs GameTarget (AdditionalStagingReveals (-1))
      pure e
    _ -> GildorsCounsel <$> runMessage msg attrs
