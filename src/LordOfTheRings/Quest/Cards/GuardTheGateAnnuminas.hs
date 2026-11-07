module LordOfTheRings.Quest.Cards.GuardTheGateAnnuminas (guardTheGateAnnuminas) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types (EpicEffect (EpicGrantHeroResources))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers (annuminasVictoryCopies, chooseAnnuminasStage)

newtype GuardTheGateAnnuminas = GuardTheGateAnnuminas QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

guardTheGateAnnuminas :: QuestCard GuardTheGateAnnuminas
guardTheGateAnnuminas = quest GuardTheGateAnnuminas Cards.guardTheGateAnnuminas

instance HasModifiersFor GuardTheGateAnnuminas where
  getModifiersFor (GuardTheGateAnnuminas a) = when a.flipped do
    hosts <- annuminasVictoryCopies $ toCardCode Enemies.hostOfAngmar
    modifySelf a [QuestPointModifier $ negate $ 10 * hosts]
    modified a GameTarget [AdditionalStagingReveals 1]

instance HasAbilities GuardTheGateAnnuminas where
  getAbilities (GuardTheGateAnnuminas a) = [mkAbility a 1 $ response RoundEndsWindow | a.flipped]

instance RunMessage GuardTheGateAnnuminas where
  runMessage msg q@(GuardTheGateAnnuminas a) = case msg of
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      askToResolveWhenRevealed a
      GuardTheGateAnnuminas <$> runMessage msg a
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
      chooseAnnuminasStage msg pid a EpicGrantHeroResources
      pure q
    _ -> GuardTheGateAnnuminas <$> runMessage msg a
