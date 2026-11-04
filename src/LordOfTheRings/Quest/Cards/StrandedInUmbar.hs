module LordOfTheRings.Quest.Cards.StrandedInUmbar (strandedInUmbar) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EscapeFromUmbar.Helpers (fleeing)

newtype StrandedInUmbar = StrandedInUmbar QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

strandedInUmbar :: QuestCard StrandedInUmbar
strandedInUmbar = quest StrandedInUmbar Cards.strandedInUmbar

-- | This stage cannot have more than 15 progress tokens on it. While at least 1 player is
-- engaged with an enemy, the players cannot defeat this stage.
instance HasModifiersFor StrandedInUmbar where
  getModifiersFor (StrandedInUmbar a) = fleeing a

{- | Setup: Add "Seize Them!" to the staging area. Each player adds 1 different location to
the staging area and puts 1 copy of Umbar Sentry into play engaged with him. Shuffle the
encounter deck.

Each player in turn, from the first player, searches for a location unlike those already in
the staging area, then takes his Umbar Sentry.
-}
instance RunMessage StrandedInUmbar where
  runMessage msg q@(StrandedInUmbar attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (find (cardMatches $ cardIs Objectives.seizeThem) deck) \card -> do
        removeFromEncounterDeck card
        addToStaging card
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
          deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
          for_ (find (cardMatches $ cardIs Enemies.umbarSentry) deck) \card -> do
            removeFromEncounterDeck card
            putEnemyIntoPlayEngaged pid card
          doStep (n + 1) (PerformSetup $ toTarget attrs)
        [] -> shuffleEncounterDeck
      pure q
    _ -> StrandedInUmbar <$> runMessage msg attrs
