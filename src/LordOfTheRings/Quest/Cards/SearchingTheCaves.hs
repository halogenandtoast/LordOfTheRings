module LordOfTheRings.Quest.Cards.SearchingTheCaves (searchingTheCaves) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype SearchingTheCaves = SearchingTheCaves QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

searchingTheCaves :: QuestCard SearchingTheCaves
searchingTheCaves = quest SearchingTheCaves Cards.searchingTheCaves

{- | Setup: Set Brood Mother aside, out of play. Add the Search objective to the staging
area (Off Track faceup).
When Revealed (1B): Discard cards from the encounter deck until X locations are discarded,
where X is the number of players. Add each location discarded by this effect to the
staging area.
-}
instance RunMessage SearchingTheCaves where
  runMessage msg q@(SearchingTheCaves attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      traverse_ setAside $ filter (cardMatches $ cardIs Enemies.broodMother) deck
      for_ (find (cardMatches $ cardIs Objectives.offTrack) deck) \c -> do
        removeFromEncounterDeck c
        addToStaging c
      askToResolveWhenRevealed attrs
      SearchingTheCaves <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players (CardWithType LocationType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ filter (cardMatches $ CardWithType LocationType) cards
      pure q
    _ -> SearchingTheCaves <$> runMessage msg attrs
