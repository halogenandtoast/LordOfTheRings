module LordOfTheRings.Quest.Cards.TheLeapingFishQuest (theLeapingFishQuest) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheLeapingFishQuest = TheLeapingFishQuest QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLeapingFishQuest :: QuestCard TheLeapingFishQuest
theLeapingFishQuest = quest TheLeapingFishQuest Cards.theLeapingFishQuest

-- | Setup: Search the encounter deck for The Leaping Fish and Alcaron's Scroll. Make The
-- Leaping Fish the active location and attach Alcaron's Scroll to a hero.
-- When Revealed: Each player must search the encounter deck for a copy of Harbor Thug and
-- add it to the staging area. Shuffle the encounter deck.
instance RunMessage TheLeapingFishQuest where
  runMessage msg q@(TheLeapingFishQuest attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let find' def = find ((== toCardCode def) . toCardCode) deck
      for_ (find' Locations.theLeapingFish) \fish -> do
        removeFromEncounterDeck fish
        putIntoPlayAsActiveLocation fish
      for_ (find' Objectives.alcaronsScroll) \scroll -> do
        removeFromEncounterDeck scroll
        lead <- getLead
        heroes <- select AnyHero
        chooseOrRunOneM lead do
          questionLabeled "alcaronsScroll.attach"
          causedBy attrs
          targets heroes (putObjectiveIntoPlayAttached scroll)
      turnOver attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      eachPlayer \p -> searchEncounterDeckToStaging p (cardIs Enemies.harborThug)
      pure $ TheLeapingFishQuest $ attrs {questFlipped = True}
    _ -> TheLeapingFishQuest <$> runMessage msg attrs
