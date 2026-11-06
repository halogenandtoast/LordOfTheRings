module LordOfTheRings.Quest.Cards.TheLivingForest
  (darkBadPlace, chokedWithBrambles, shiftingTrees, closingInAroundThem) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Helpers.Window (exhaustableHeroes)
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheOldForest.Helpers

newtype TheLivingForest = TheLivingForest QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

darkBadPlace, chokedWithBrambles, shiftingTrees, closingInAroundThem :: QuestCard TheLivingForest
darkBadPlace = quest TheLivingForest Cards.darkBadPlace
chokedWithBrambles = quest TheLivingForest Cards.chokedWithBrambles
shiftingTrees = quest TheLivingForest Cards.shiftingTrees
closingInAroundThem = quest TheLivingForest Cards.closingInAroundThem

instance HasModifiersFor TheLivingForest where
  getModifiersFor (TheLivingForest a) = when a.flipped do
    modifySelf a [CannotBeDefeated]
    when (toCardCode a == toCardCode Cards.closingInAroundThem) $ modified a GameTarget [AdditionalStagingReveals 1]

instance HasAbilities TheLivingForest where
  getAbilities (TheLivingForest a) =
    [mkAbility a 1 $ forced $ PlacedProgressFromQuestingWindow After | a.flipped]
      <> [mkAbility a 2 $ forced $ PhaseBeginsWindow After PlanningPhase | a.flipped, toCardCode a == toCardCode Cards.darkBadPlace]
      <> [mkAbility a 3 $ forced RoundBeginsWindow | a.flipped, toCardCode a == toCardCode Cards.chokedWithBrambles]
      <> [mkAbility a 4 $ forced RoundEndsWindow | a.flipped, toCardCode a == toCardCode Cards.shiftingTrees]

instance RunMessage TheLivingForest where
  runMessage msg q@(TheLivingForest a) = case msg of
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      askToResolveWhenRevealed a
      TheLivingForest <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      locations <- select LocationInStagingArea
      players <- getPlayerOrder
      if length locations < length players
        then do
          lead <- getLead
          searchEncounterDeckAndDiscard lead a (CardWithType LocationType)
        else shuffleEncounterDeck
      pure q
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      points <- sum <$> (traverse locationVictoryPoints =<< victoryLocations)
      players <- length <$> getPlayerOrder
      advanceForestStage a (points >= 5 * players)
      pure q
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      eachPlayer \pid -> do
        heroes <- exhaustableHeroes $ HeroControlledBy (PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "oldForest.exhaustHero"
          causedBy a
          targets heroes exhaust
      pure q
    UseCardAbility _ (isSource a -> True) 3 _ -> do
      locations <- select AnyLocation
      for_ locations \lid -> do
        location <- getLocation lid
        removeTokens a lid Progress location.progress
      pure q
    UseCardAbility _ (isSource a -> True) 4 _ -> q <$ void discardRandomVictoryLocation
    _ -> TheLivingForest <$> runMessage msg a
