module LordOfTheRings.Event.Cards.TheCannyHuntsman (theCannyHuntsman) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Attachment (heroesWithAttached)
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype TheCannyHuntsman = TheCannyHuntsman EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | A Song boon of the Ered Mithrin campaign.
theCannyHuntsman :: EventCard TheCannyHuntsman
theCannyHuntsman = eventWith TheCannyHuntsman Cards.theCannyHuntsman (criteriaL .~ DuringPhase QuestPhase)

{- | Quest Action: Look at the top 5 cards of the encounter deck. Put any number of those
cards on top of the encounter deck and the rest on the bottom in any order. The hero with
Master of the Hunt attached gets +3 [willpower] until the end of the phase.
-}
instance RunMessage TheCannyHuntsman where
  runMessage msg e@(TheCannyHuntsman attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      top <- take 5 . (.encounterDeck) . toAttrs <$> getsGame (.scenario)
      -- each in turn goes on the top or the bottom: the last put on top ends up on top, the
      -- last put on the bottom lowest, so any order can be had
      chooseOneAtATimeM pid do
        questionLabeled "theCannyHuntsman.putBack"
        causedBy attrs
        showing (map EncounterCard top)
        targets top \c -> chooseOneM pid do
          questionLabeled "theCannyHuntsman.topOrBottom"
          causedBy attrs
          showing [EncounterCard c]
          labeled "theCannyHuntsman.onTop" do
            removeFromEncounterDeck c
            putOnTopOfEncounterDeck c
          labeled "theCannyHuntsman.onBottom" do
            removeFromEncounterDeck c
            push $ PutOnBottomOfEncounterDeck [c]
      hunters <- heroesWithAttached Attachments.masterOfTheHunt
      for_ hunters \h -> phaseModifier attrs h (WillpowerModifier 3)
      pure e
    _ -> TheCannyHuntsman <$> runMessage msg attrs
