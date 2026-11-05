module LordOfTheRings.Event.Cards.LayOfCourage (layOfCourage) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Attachment (heroesWithAttached)
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype LayOfCourage = LayOfCourage EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | A Song boon of the Ered Mithrin campaign. Quest Response: Cancel the "when revealed"
-- effects of a card that was just revealed from the encounter deck.
layOfCourage :: EventCard LayOfCourage
layOfCourage =
  eventWith LayOfCourage Cards.layOfCourage ((windowL ?~ CardRevealedWindow When) . (criteriaL .~ (DuringPhase QuestPhase <> whenRevealedCancelable)))

-- | Then: Ready the hero with The Steadfast attached. Add Lay of Courage to the victory
-- display.
instance RunMessage LayOfCourage where
  runMessage msg e@(LayOfCourage attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      for_ [t | Window _ (Revealed t) <- windows] (cancelWhenRevealed pid)
      traverse_ ready =<< heroesWithAttached Attachments.theSteadfast
      addToVictoryDisplay card
      pure e
    _ -> LayOfCourage <$> runMessage msg attrs
