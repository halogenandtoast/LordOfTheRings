module LordOfTheRings.Quest.Cards.ExploreTheIsland (exploreTheIsland) where

import LordOfTheRings.Card (cdQuestPoints, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ExploreTheIsland = ExploreTheIsland QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

exploreTheIsland :: QuestCard ExploreTheIsland
exploreTheIsland = quest ExploreTheIsland Cards.exploreTheIsland

-- | Forced: After a location is explored, add progress to Explore the Island equal to that
-- location's printed quest points.
-- Forced: When an Uncharted location becomes the active location, add 1 copy of Lost
-- Island from the top of the Uncharted deck to the staging area.
-- ponytail: the first answers as the location is explored, while it is still in play to
-- read its quest points from
instance HasAbilities ExploreTheIsland where
  getAbilities (ExploreTheIsland a) =
    [ ability
    | a.flipped
    , ability <-
        [ mkAbility a 1 $ forced $ AnyExploredWindow When
        , mkAbility a 2 $ forced $ LocationBecameActiveWindow When unchartedLocation
        ]
    ]

-- | Setup: Prepare the Uncharted deck. Set Shrine to Morgoth aside, out of play. Add 2
-- copies of Lost Island from the top of the Uncharted deck to the staging area (3 copies
-- instead if there are 3 or 4 players in the game). The first player takes control of
-- Calphon. The scenario made the Uncharted deck, and set Shrine to Morgoth aside, as it
-- gathered the encounter deck.
-- Progress cannot be placed on Explore the Island except by quest effects.
instance RunMessage ExploreTheIsland where
  runMessage msg q@(ExploreTheIsland attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      players <- length <$> getRemainingPlayers
      addFromUnchartedDeck (if players >= 3 then 3 else 2)
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (filter ((== toCardCode Objectives.calphon) . toCardCode) deck) \c -> do
        removeFromEncounterDeck c
        addToStaging c
      ExploreTheIsland <$> runMessage msg attrs
    PlaceTokens source (isTarget attrs -> True) Progress _ | not (isSource attrs source) -> pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [lid | Window _ (Explored lid) <- ws] \lid -> do
        l <- getLocation lid
        placeProgress (AbilitySource (toSource attrs) 1) attrs (fromMaybe 0 $ cdQuestPoints $ toCardDef l)
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      addFromUnchartedDeck 1
      pure q
    _ -> ExploreTheIsland <$> runMessage msg attrs
