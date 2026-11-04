module LordOfTheRings.Quest.Cards.SearchingForUlchor (searchingForUlchor) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast.Helpers

newtype SearchingForUlchor = SearchingForUlchor QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

searchingForUlchor :: QuestCard SearchingForUlchor
searchingForUlchor = quest SearchingForUlchor Cards.searchingForUlchor

-- | City Guard cannot have more than 5 damage, and it gets -5 engagement cost for each
-- resource here.
instance HasModifiersFor SearchingForUlchor where
  getModifiersFor (SearchingForUlchor a) = cityGuardStageModifiers a 5

-- | Forced: At the end of the round, place 1 resource here.
instance HasAbilities SearchingForUlchor where
  getAbilities (SearchingForUlchor a) = cityGuardStageAbilities a

{- | Setup: Add City Guard to the staging area. Each player adds a different location to the
staging area. Shuffle the encounter deck.
The scenario set City Guard aside as it gathered the encounter deck. Each player searches
in turn, from the first player, for a location unlike those already in the staging area.
-}
instance RunMessage SearchingForUlchor where
  runMessage msg q@(SearchingForUlchor attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.cityGuard)
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
    _ -> SearchingForUlchor <$> runCityGuardStage msg attrs
