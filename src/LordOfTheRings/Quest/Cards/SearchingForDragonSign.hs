module LordOfTheRings.Quest.Cards.SearchingForDragonSign (searchingForDragonSign) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWitheredHeath.Helpers (dragonSignsInVictoryDisplay)

newtype SearchingForDragonSign = SearchingForDragonSign QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

searchingForDragonSign :: QuestCard SearchingForDragonSign
searchingForDragonSign = quest SearchingForDragonSign Cards.searchingForDragonSign

-- | The players cannot defeat this stage unless at least 1 Dragon Sign is in the victory
-- display.
instance HasModifiersFor SearchingForDragonSign where
  getModifiersFor (SearchingForDragonSign a) = when a.flipped do
    signs <- dragonSignsInVictoryDisplay
    when (signs < 1) $ modifySelf a [CannotBeDefeated]

-- | Forced: After the active location is explored, look at the top 3 cards of the
-- encounter deck. Reveal 1 of those cards and discard the rest.
instance HasAbilities SearchingForDragonSign where
  getAbilities (SearchingForDragonSign a) = [mkAbility a 1 $ forced $ ActiveLocationExploredWindow After | a.flipped]

{- | Setup: Set Cold-Drake and all four copies of Dragon Sign aside, out of play. Create the
Caves deck and set it next to the quest deck. Shuffle 2 set aside Dragon Sign objectives
into the Caves deck. Shuffle the encounter deck and discard cards from the top until X
locations are discarded, where X is the number of players. Add each discarded location to
the staging area.
The scenario set the cards aside and made the Caves deck as it gathered the encounter
deck. The first player picks which of the 3 cards is revealed.
-}
instance RunMessage SearchingForDragonSign where
  runMessage msg q@(SearchingForDragonSign attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      signs <- getSetAside (cardIs Objectives.dragonSign)
      for_ (take 2 signs) shuffleIntoCavesDeck
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players locationCard
      SearchingForDragonSign <$> runMessage msg attrs
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (filter (cardMatches locationCard) cards) addToStagingFromDiscard
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      revealOneOfTopEncounterCards lead attrs 3
      pure q
    _ -> SearchingForDragonSign <$> runMessage msg attrs
   where
    locationCard = CardWithType LocationType
