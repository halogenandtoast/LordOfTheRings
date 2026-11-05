module LordOfTheRings.Quest.Cards.AFatefulDiscovery (aFatefulDiscovery) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (cdVictoryPoints, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesLocations)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (isUnchartedCard, locationDeckCard)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AFatefulDiscovery = AFatefulDiscovery QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aFatefulDiscovery :: QuestCard AFatefulDiscovery
aFatefulDiscovery = quest AFatefulDiscovery Cards.aFatefulDiscovery

{- | When Revealed: Add 2 copies of Lost Island from the top of the Uncharted deck to the
staging area. Add Shrine to Morgoth to the staging area, Lost Island side faceup. Remove
all progress from each Uncharted location in the staging area and shuffle them so that
the players do not know which one is which.
Forced: After a location is explored, if it would be shuffled into the Uncharted deck, add
it to the victory display, instead: as it is explored, where it would go is changed.
The players cannot defeat this stage unless Shrine to Morgoth is in the victory display.
When Shrine to Morgoth is added to the victory display, the players win the game.
-}
instance RunMessage AFatefulDiscovery where
  runMessage msg q@(AFatefulDiscovery attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      AFatefulDiscovery <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addFromUnchartedDeck 2
      addSetAsideToStaging (CardMatchAny $ map cardIs Locations.fateOfNumenorShrines)
      shuffleUnchartedLocations
      pure q
    CheckWindows ws | attrs.flipped -> do
      locations <- getsGame (entitiesLocations . gameEntities)
      for_ [l | Window When (Explored lid) <- ws, Just l <- [Map.lookup lid locations], isUnchartedCard (toAttrs l)] \l -> do
        cancelMatching \case
          Discard _ (LocationTarget lid) -> lid == l.id
          ShuffleIntoUnchartedDeck lid -> lid == l.id
          _ -> False
        -- one with victory points already goes there
        when (isNothing $ cdVictoryPoints $ toCardDef l) $ addToVictoryDisplay l
      AFatefulDiscovery <$> runMessage msg attrs
    AddToVictoryDisplay (LocationTarget lid) | attrs.flipped -> do
      locations <- getsGame (entitiesLocations . gameEntities)
      let shrine l = toCardCode (locationDeckCard $ toAttrs l) `elem` map toCardCode Locations.fateOfNumenorShrines
      when (maybe False shrine (Map.lookup lid locations)) winGame
      AFatefulDiscovery <$> runMessage msg attrs
    _ -> AFatefulDiscovery <$> runMessage msg attrs
