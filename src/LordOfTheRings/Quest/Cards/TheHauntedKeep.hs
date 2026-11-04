module LordOfTheRings.Quest.Cards.TheHauntedKeep (theHauntedKeep) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (lootStage)

newtype TheHauntedKeep = TheHauntedKeep QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHauntedKeep :: QuestCard TheHauntedKeep
theHauntedKeep = quest TheHauntedKeep Cards.theHauntedKeep

-- | This stage cannot be defeated unless the players control at least 1 Loot objective.
instance HasModifiersFor TheHauntedKeep where
  getModifiersFor (TheHauntedKeep a) = when a.flipped $ lootStage a 1

{- | Setup: Set The Cursed Tomb aside, out of play. Each player searches the encounter deck
for a different location and adds it to the staging area. Shuffle the encounter deck.
The scenario set The Cursed Tomb aside as it gathered the deck. Each player searches in
turn, from the first player, for a location unlike those already in the staging area.
-}
instance RunMessage TheHauntedKeep where
  runMessage msg q@(TheHauntedKeep attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      doStep 1 msg
      turnOver attrs
      pure q
    DoStep n (PerformSetup (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      case drop (n - 1) players of
        pid : _ -> do
          staged <-
            getsGame
              (map (CardWithCode . toCardCode) . filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))
          searchEncounterDeckToStaging pid (CardWithType LocationType <> NotCard (CardMatchAny staged))
          doStep (n + 1) (PerformSetup $ toTarget attrs)
        [] -> shuffleEncounterDeck
      pure q
    _ -> TheHauntedKeep <$> runMessage msg attrs
