module LordOfTheRings.Quest.Cards.IntoTheEttenmoors (intoTheEttenmoors) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Keyword (Keyword (Safe))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers

newtype IntoTheEttenmoors = IntoTheEttenmoors QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

intoTheEttenmoors :: QuestCard IntoTheEttenmoors
intoTheEttenmoors = quest IntoTheEttenmoors Cards.intoTheEttenmoors

-- | Forced: At the beginning of the quest phase, if there are no encounter side quests in
-- play, discard cards from the top of the encounter deck until a side quest is discarded,
-- and add it to the staging area.
instance HasAbilities IntoTheEttenmoors where
  getAbilities (IntoTheEttenmoors a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]

-- | Setup: Set aside each of the 3 Across the Ettenmoors side quests and each of the 4
-- locations with the safe keyword. Reveal 1 of those side quests at random and 1 of those
-- safe locations at random, adding them to the staging area with the safe location guarded
-- by the side quest. Search the encounter deck for 1 copy of Savage Trollspawn per player
-- and add them to the staging area. Shuffle the remaining side quests and safe locations
-- back into the encounter deck. The first player gains control of Amarthiúl.
-- The others never leave the encounter deck, which is shuffled.
-- Forced: When a side quest is defeated, place progress tokens on Into the Ettenmoors
-- equal to that side quest's quest points.
instance RunMessage IntoTheEttenmoors where
  runMessage msg q@(IntoTheEttenmoors attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      players <- length <$> getRemainingPlayers
      let ours = [Cards.lieLow, Cards.forageForFood, Cards.scavengeForSupplies]
          sideQuests = filter (\c -> any (`cardMatches` c) (map cardIs ours)) deck
          safe = filter (member Safe . cdKeywords . toCardDef) deck
      unless (null sideQuests) do
        sideQuest <- uniform sideQuests
        removeFromEncounterDeck sideQuest
        addToStaging sideQuest
        unless (null safe) do
          location <- uniform safe
          removeFromEncounterDeck location
          putLocationIntoPlay location (AttachedTo $ CardIdTarget sideQuest.id)
      for_ (take players $ filter (cardMatches $ cardIs Enemies.savageTrollspawn) deck) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      -- Amarthiúl joins the first player as he comes out
      for_ (take 1 $ filter (cardMatches $ cardIs Objectives.amarthiulAcrossTheEttenmoors) deck) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      shuffleEncounterDeck
      pure $ IntoTheEttenmoors $ attrs {questFlipped = True}
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      none <- null <$> getEncounterSideQuests
      when none $ discardUntilSideQuest attrs
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard (sideQuestsAmong cards)
      pure q
    CompleteQuest qid | qid /= attrs.id -> do
      defeated <- getsGame (find ((== qid) . toId) . filter isSideQuest . toList . (.entities.quests))
      for_ defeated \s -> placeProgress attrs attrs s.questPoints
      IntoTheEttenmoors <$> runMessage msg attrs
    _ -> IntoTheEttenmoors <$> runMessage msg attrs
