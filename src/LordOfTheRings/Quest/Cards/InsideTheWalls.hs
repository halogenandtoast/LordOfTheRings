module LordOfTheRings.Quest.Cards.InsideTheWalls (insideTheWalls) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.ControlledLocation (firstPlayerTakesControl)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype InsideTheWalls = InsideTheWalls QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

insideTheWalls :: QuestCard InsideTheWalls
insideTheWalls = quest InsideTheWalls Cards.insideTheWalls

-- | The stage gets +4 quest points per player. Progress cannot be placed on locations in
-- the staging area by player card effects.
instance HasModifiersFor InsideTheWalls where
  getModifiersFor (InsideTheWalls a) = when a.flipped do
    players <- length <$> getRemainingPlayers
    modifySelf a [QuestPointModifier (4 * players)]
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [NoProgressFromPlayerCards]

-- | The stage cannot be defeated unless there are more locations under the players'
-- control than in the staging area at the end of the round: it is checked then, and only
-- then.
instance HasAbilities InsideTheWalls where
  getAbilities (InsideTheWalls a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

{- | Setup: Add Thane Ulchor and Gate of Dorwinion to the staging area. Each player adds a
different location to the staging area. Make a copy of Makeshift Fortification the active
location. Shuffle the encounter deck.

Each player in turn, from the first player, searches for a location unlike those already in
the staging area.
Forced: When a location is explored, the first player takes control of it.
-}
instance RunMessage InsideTheWalls where
  runMessage msg q@(InsideTheWalls attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ [Enemies.thaneUlchorWrathAndRuin, Locations.gateOfDorwinion] \def ->
        for_ (find (cardMatches $ cardIs def) deck) \card -> do
          removeFromEncounterDeck card
          addToStaging card
      doStep 1 msg
      turnOver attrs
      pure q
    DoStep n inner@(PerformSetup (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      case drop (n - 1) players of
        pid : _ -> do
          staged <-
            getsGame
              (map (CardWithCode . toCardCode) . filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))
          searchEncounterDeckToStaging pid (CardWithType LocationType <> NotCard (CardMatchAny staged))
          doStep (n + 1) inner
        [] -> do
          deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
          for_ (find (cardMatches $ cardIs Locations.makeshiftFortification) deck) \card -> do
            removeFromEncounterDeck card
            putIntoPlayAsActiveLocation card
          shuffleEncounterDeck
      pure q
    CheckWindows ws | attrs.flipped -> do
      for_ [lid | Window When (Explored lid) <- ws] firstPlayerTakesControl
      InsideTheWalls <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      controlled <- length <$> select (LocationControlledBy AnyPlayer)
      staged <- length <$> select LocationInStagingArea
      done <- hasEnoughProgress attrs
      when (done && controlled > staged) $ completeQuest attrs.id
      pure q
    _ -> InsideTheWalls <$> runConditionalStage (pure False) msg attrs
