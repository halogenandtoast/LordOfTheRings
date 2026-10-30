module LordOfTheRings.Quest.Cards.CorneredAnimals (corneredAnimals) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWeatherHills.Helpers

newtype CorneredAnimals = CorneredAnimals QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

corneredAnimals :: QuestCard CorneredAnimals
corneredAnimals = quest CorneredAnimals Cards.corneredAnimals

-- | Each damaged character gets -1 willpower.
instance HasModifiersFor CorneredAnimals where
  getModifiersFor (CorneredAnimals a) = when a.flipped do
    damaged <- damagedCharacters AnyHero AnyAlly
    for_ damaged \c -> modified a c [WillpowerModifier (-1)]

-- | Forced: At the beginning of the quest phase, either discard 1 resource token from
-- Savage Counter-attack, or reveal an additional encounter card this phase.
instance HasAbilities CorneredAnimals where
  getAbilities (CorneredAnimals a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase | a.flipped]

-- | When Revealed: End the current phase. Shuffle the encounter discard pile and the Orc
-- deck into the encounter deck. Reveal Orc Ambush and Amon Forn and add them to the
-- staging area.
-- If the players defeat this stage, they win the game: not while Amon Forn is in play, as
-- it reads, so the stage's progress piles up until Amon Forn leaves play.
instance RunMessage CorneredAnimals where
  runMessage msg q@(CorneredAnimals attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      CorneredAnimals <$> runStage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      endCurrentPhase
      shuffleEncounterDiscardIntoDeck AnyCard
      shuffleOrcDeckIntoEncounterDeck
      revealSetAsideMatching orcAmbush
      revealSetAsideMatching (cardIs Locations.amonForn)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      mission <- getMissionObjective
      tokens <- missionResources
      chooseOrRunOneM lead do
        questionLabeled "corneredAnimals.choose"
        causedBy attrs
        for_ mission \m ->
          when (tokens > 0) $ labeled "corneredAnimals.discard" $ removeTokens attrs m Resource 1
        labeled "corneredAnimals.reveal" $ phaseModifier attrs GameTarget (AdditionalStagingReveals 1)
      pure q
    _ -> CorneredAnimals <$> runStage msg attrs
   where
    runStage = runConditionalStage (not <$> selectAny (LocationWithCardCode $ toCardCode Locations.amonForn))
