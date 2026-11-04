module LordOfTheRings.Quest.Cards.ChasedByEasterlings (chasedByEasterlings) where

import LordOfTheRings.Card (CardType (EnemyType), cdName, toCardDef)
import LordOfTheRings.Name (Name (Name))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRiverRunning.Helpers (getEasterlingPursuit)
import LordOfTheRings.Trait (Trait (Easterling))

newtype ChasedByEasterlings = ChasedByEasterlings QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

chasedByEasterlings :: QuestCard ChasedByEasterlings
chasedByEasterlings = quest ChasedByEasterlings Cards.chasedByEasterlings

-- | This stage gets +5 quest points per player.
-- Add +1 Threat to the total threat in the staging area for each resource token on
-- Easterling Pursuit.
instance HasModifiersFor ChasedByEasterlings where
  getModifiersFor (ChasedByEasterlings a) = when a.flipped do
    players <- length <$> getRemainingPlayers
    modifySelf a [QuestPointModifier (5 * players)]
    getEasterlingPursuit >>= traverse_ \o -> modified a GameTarget [StagingThreatPerToken (ObjectiveTarget o) Resource]

{- | Setup: Set Warriors of the East aside, out of play. Add Easterling Pursuit and 1 copy of
River Running to the staging area. Each player adds 1 different Easterling enemy to the
staging area. Shuffle the encounter deck.

The scenario set Warriors of the East aside as it gathered the encounter deck. Each player
searches in turn, from the first player, for an Easterling enemy whose title is unlike
those already in the staging area; the deck is shuffled after each search.
-}
instance RunMessage ChasedByEasterlings where
  runMessage msg q@(ChasedByEasterlings attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let firstOf def = take 1 (filter (cardMatches $ cardIs def) deck)
      for_ (concatMap firstOf [Objectives.easterlingPursuit, Locations.riverRunning]) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      doStep 1 msg
      pure q
    DoStep n (PerformSetup (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      case drop (n - 1) players of
        pid : _ -> do
          staged <- getsGame (map (\e -> let Name t _ = cdName (toCardDef e) in CardWithTitle t) . filter ((== StagingArea) . (.placement)) . toList . (.entities.enemies))
          searchEncounterDeckToStaging pid (CardWithType EnemyType <> CardWithTrait Easterling <> NotCard (CardMatchAny staged))
          doStep (n + 1) (PerformSetup $ toTarget attrs)
        [] -> do
          shuffleEncounterDeck
          turnOver attrs
      pure q
    _ -> ChasedByEasterlings <$> runMessage msg attrs
