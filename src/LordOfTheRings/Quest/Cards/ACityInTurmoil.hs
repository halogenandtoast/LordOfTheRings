module LordOfTheRings.Quest.Cards.ACityInTurmoil (aCityInTurmoil) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DangerInDorwinion.Helpers (questResources)

newtype ACityInTurmoil = ACityInTurmoil QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

aCityInTurmoil :: QuestCard ACityInTurmoil
aCityInTurmoil = quest ACityInTurmoil Cards.aCityInTurmoil

-- | This stage cannot be defeated unless there are at least 2 resources here.
instance HasModifiersFor ACityInTurmoil where
  getModifiersFor (ACityInTurmoil a) = when (a.flipped && questResources a < 2) $ modifySelf a [CannotBeDefeated]

-- | Forced: After the active location leaves play as an explored location, place 1
-- resource here.
instance HasAbilities ACityInTurmoil where
  getAbilities (ACityInTurmoil a) = [mkAbility a 1 $ forced $ ActiveLocationExploredWindow After | a.flipped]

{- | Setup: Set each plot objective and each Cultist enemy aside, out of play. Each player
searches the encounter deck for a different location and adds it to the staging area. One
of those locations must be Side Street. Shuffle the encounter deck.
The scenario set the Plot objectives and Cultist enemies aside as it gathered the encounter
deck. Which player found which location does not matter, so the first player's search
finds Side Street, and each other player in turn order finds a location with a title not
yet in the staging area.
-}
instance RunMessage ACityInTurmoil where
  runMessage msg q@(ACityInTurmoil attrs) = case msg of
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
          searchEncounterDeckToStaging pid
            $ if n == 1
              then cardIs Locations.sideStreet
              else CardWithType LocationType <> NotCard (CardMatchAny staged)
          doStep (n + 1) (PerformSetup $ toTarget attrs)
        [] -> shuffleEncounterDeck
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      pure q
    _ -> ACityInTurmoil <$> runMessage msg attrs
