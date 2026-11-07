module LordOfTheRings.Quest.Cards.AnnuminasDefense (guardTheGateAnnuminas, defendTheCityAnnuminas) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types (EpicEffect (EpicGrantHeroResources, EpicHealControlledCharacter), EpicTargetQuery (EpicHealingGroup))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers (annuminasVictoryCopies, chooseAnnuminasStage)

newtype AnnuminasDefense = AnnuminasDefense QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

guardTheGateAnnuminas, defendTheCityAnnuminas :: QuestCard AnnuminasDefense
guardTheGateAnnuminas = quest AnnuminasDefense Cards.guardTheGateAnnuminas
defendTheCityAnnuminas = quest AnnuminasDefense Cards.defendTheCityAnnuminas

instance HasModifiersFor AnnuminasDefense where
  getModifiersFor (AnnuminasDefense a) = when a.flipped do
    hosts <- annuminasVictoryCopies $ toCardCode Enemies.hostOfAngmar
    modifySelf a [QuestPointModifier $ negate $ 10 * hosts]
    when (toCardCode a == toCardCode Cards.guardTheGateAnnuminas) $
      modified a GameTarget [AdditionalStagingReveals 1]

instance HasAbilities AnnuminasDefense where
  getAbilities (AnnuminasDefense a) =
    [restricted a 1
      (if toCardCode a == toCardCode Cards.defendTheCityAnnuminas then EpicTargetsExist EpicHealingGroup else NoCriteria)
      $ response RoundEndsWindow | a.flipped]
      <> [mkAbility a 2 $ forced $ AllPlayersCommittedToQuestWindow After
         | a.flipped, toCardCode a == toCardCode Cards.defendTheCityAnnuminas]

instance RunMessage AnnuminasDefense where
  runMessage msg q@(AnnuminasDefense a) = case msg of
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      askToResolveWhenRevealed a
      AnnuminasDefense <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget a -> True)) -> do
      players <- getRemainingPlayers
      discardEncounterCardsUntilCount a (max 1 $ length players - 1) (CardWithType EnemyType)
      pure q
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ filter (cardMatches $ CardWithType EnemyType) cards
      pure q
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      chooseAnnuminasStage msg pid a $
        if toCardCode a == toCardCode Cards.guardTheGateAnnuminas then EpicGrantHeroResources else EpicHealControlledCharacter
      pure q
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      staged <- select EnemyInStagingArea
      values <- traverse (\eid -> (eid,) <$> getEnemyAttack eid) staged
      let highest = maximumMay $ map snd values
          candidates = [eid | (eid, value) <- values, Just value == highest]
      players <- getRemainingPlayers
      lead <- getLead
      case candidates of
        [] -> revealEncounterCard
        _ -> chooseOrRunOneM lead do
          questionLabeled "annuminas.highestAttack"
          causedBy a
          targets candidates $ \eid -> push $ EnemyAttacksPlayers eid players False [RevealEncounterCard]
      pure q
    _ -> AnnuminasDefense <$> runMessage msg a
