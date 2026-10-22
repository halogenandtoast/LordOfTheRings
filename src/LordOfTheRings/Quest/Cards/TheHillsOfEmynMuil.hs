module LordOfTheRings.Quest.Cards.TheHillsOfEmynMuil (theHillsOfEmynMuil) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (EmynMuil))

newtype TheHillsOfEmynMuil = TheHillsOfEmynMuil QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHillsOfEmynMuil :: QuestCard TheHillsOfEmynMuil
theHillsOfEmynMuil = quest TheHillsOfEmynMuil Cards.theHillsOfEmynMuil

-- | Forced: If there are no location cards in the staging area, the first treachery card
-- revealed during the quest phase gains surge. Only that first treachery is looked at.
instance HasAbilities TheHillsOfEmynMuil where
  getAbilities (TheHillsOfEmynMuil a) =
    [limited (GroupLimit PerPhase 1) $ restricted a 1 (DuringPhase QuestPhase) $ silent $ TreacheryRevealedWindow After]

-- | Setup: Search the encounter deck for Amon Hen and Amon Lhaw, and add them to the
-- staging area. Then shuffle the encounter deck.
-- Players cannot defeat this stage unless there are no Emyn Muil locations in play, and
-- they have collected at least 20 victory points.
instance RunMessage TheHillsOfEmynMuil where
  runMessage msg q@(TheHillsOfEmynMuil attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      lead <- getLead
      searchEncounterDeckToStaging lead (cardIs Locations.amonHen)
      searchEncounterDeckToStaging lead (cardIs Locations.amonLhaw)
      pure $ TheHillsOfEmynMuil $ attrs {questFlipped = True}
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      whenM (selectNone LocationInStagingArea) revealEncounterCard
      pure q
    _ -> TheHillsOfEmynMuil <$> runConditionalStage condition msg attrs
   where
    condition = do
      victory <- getsGame ((.victoryPoints) . toAttrs . (.scenario))
      andM [selectNone (LocationWithTrait EmynMuil), pure $ victory >= 20]
