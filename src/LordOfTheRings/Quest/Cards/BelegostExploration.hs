module LordOfTheRings.Quest.Cards.BelegostExploration
  ( ruinsOfTheFirstAge, ancientEvilsDisturbed, deeperIntoThePast ) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Phase (Phase (TravelPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost.Helpers

newtype BelegostExploration = BelegostExploration QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

ruinsOfTheFirstAge, ancientEvilsDisturbed, deeperIntoThePast :: QuestCard BelegostExploration
ruinsOfTheFirstAge = quest BelegostExploration Cards.ruinsOfTheFirstAge
ancientEvilsDisturbed = quest BelegostExploration Cards.ancientEvilsDisturbed
deeperIntoThePast = quest BelegostExploration Cards.deeperIntoThePast

instance HasModifiersFor BelegostExploration where
  getModifiersFor (BelegostExploration a) = when a.flipped do
    loot <- controlledLootCount
    permitted <- if toCardCode a == toCardCode Cards.ruinsOfTheFirstAge then pure (loot >= 1)
      else if toCardCode a == toCardCode Cards.ancientEvilsDisturbed
        then (loot >= 2 &&) <$> inBelegostVictory Enemies.lurkerOfTheDepths
        else pure $ loot >= 2 && countTokens Resource a.questTokens >= 3
    unless permitted $ modifySelf a [CannotBeDefeated]

instance HasAbilities BelegostExploration where
  getAbilities (BelegostExploration a) =
    [restricted a 1 (NotCriterion $ exists AnyLocation) $ forced $ PhaseBeginsWindow After TravelPhase | a.flipped, toCardCode a == toCardCode Cards.ruinsOfTheFirstAge]
    <> [mkAbility a 2 $ forced $ ActiveLocationExploredWindow After | a.flipped, toCardCode a == toCardCode Cards.deeperIntoThePast]

instance RunMessage BelegostExploration where
  runMessage msg q@(BelegostExploration a) = case msg of
    DoStep 7 Setup | toCardCode a == toCardCode Cards.ruinsOfTheFirstAge -> q <$ askToPerformSetup a
    PerformSetup (isTarget a -> True) | toCardCode a == toCardCode Cards.ruinsOfTheFirstAge -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      players <- length <$> getPlayerOrder
      let matches def c = toCardCode c == toCardCode def
          clocks = filter (matches Objectives.stalkingTheRuins) deck
          mines = take players $ filter (matches Locations.abandonedMineBelegost) deck
          aside = [Enemies.lurkerOfTheDepths, Enemies.naurlhug, Locations.naurlhugsLair, Objectives.swordOfBelegost]
      for_ (clocks <> mines) \card -> removeFromEncounterDeck card >> addToStaging card
      for_ (filter (\c -> any (\def -> matches def c) aside) deck) setAside
      doStep 1 msg
      pure q
    DoStep 1 (PerformSetup (isTarget a -> True)) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      hidden <- headMay <$> shuffleM (filter (\c -> any (\def -> toCardCode c == toCardCode def) [Objectives.mapOfEredLuin, Objectives.keysOfBelegost, Objectives.bookOfAulesChildren, Objectives.dwarvenTorch, Objectives.blueMountainGem]) deck)
      clocks <- stalkingObjectives
      for_ hidden \card -> do
        removeFromEncounterDeck card
        -- The hidden Loot is selected without showing it to any player.
        for_ clocks \o -> placeUnderneath o [EncounterCard card]
      shuffleEncounterDeck
      turnOver a
      pure q
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      when (toCardCode a /= toCardCode Cards.ruinsOfTheFirstAge) $ askToResolveWhenRevealed a
      BelegostExploration <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      if toCardCode a == toCardCode Cards.ancientEvilsDisturbed
        then addSetAsideToStaging (cardIs Enemies.lurkerOfTheDepths)
        else when (toCardCode a == toCardCode Cards.deeperIntoThePast) do
          aside <- getsGame ((.scenarioSetAside) . toAttrs . (.scenario))
          for_ (filter (\c -> toCardCode c == toCardCode Enemies.lurkerOfTheDepths) aside) \card -> do
            shuffleSetAsideIntoEncounterDeck [card]
          shuffleEncounterDiscardIntoDeck AnyCard
          doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget a -> True)) -> do
      discardEncounterCardsUntilThen a (CardWithType LocationType) \card -> do
        removeFromEncounterDiscard card
        addToStaging card
      pure q
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      locations <- select AnyLocation
      when (null locations) do
        discardPile <- getEncounterDiscard
        for_ (find (cardMatches $ CardWithType LocationType) discardPile) \card -> do
          removeFromEncounterDiscard card
          addToStaging card
      pure q
    UseCardAbility _ (isSource a -> True) 2 _ -> q <$ placeResources a a 1
    _ -> BelegostExploration <$> runMessage msg a
