module LordOfTheRings.Quest.Cards.PrancingPonyInvestigation (investigatingTheCrime, runningOutOfTime, apprehendingTheKiller) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Suspect, Hideout, Brigand))
import LordOfTheRings.Phase (Phase (QuestPhase, EncounterPhase))

newtype PrancingPonyInvestigation = PrancingPonyInvestigation QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

investigatingTheCrime, runningOutOfTime, apprehendingTheKiller :: QuestCard PrancingPonyInvestigation
investigatingTheCrime = quest PrancingPonyInvestigation Cards.investigatingTheCrime
runningOutOfTime = quest PrancingPonyInvestigation Cards.runningOutOfTime
apprehendingTheKiller = quest PrancingPonyInvestigation Cards.apprehendingTheKiller

instance HasModifiersFor PrancingPonyInvestigation where
  getModifiersFor (PrancingPonyInvestigation a) = when a.flipped do
    when (toCardCode a == toCardCode Cards.investigatingTheCrime) $ modifySelf a [ProgressPerRoundLimit 4]
    when (toCardCode a == toCardCode Cards.apprehendingTheKiller) do
      modified a GameTarget [IgnoreInvestigate, WinWhenNoMatchingEnemies $ EnemyWithTrait Suspect]
      whenM (selectAny $ LocationWithTrait Hideout) $
        select (EnemyWithTrait Suspect) >>= traverse_ (\eid -> modified a eid [CannotTakeDamage])

instance HasAbilities PrancingPonyInvestigation where
  getAbilities (PrancingPonyInvestigation a)
    | not a.flipped = []
    | toCardCode a == toCardCode Cards.investigatingTheCrime = [mkAbility a 1 $ forced $ PhaseBeginsWindow When QuestPhase]
    | toCardCode a == toCardCode Cards.runningOutOfTime =
        [mkAbility a 2 $ forced $ QuestedUnsuccessfullyWindow After, mkAbility a 3 $ forced $ QuestingProgressOnQuestWindow After a.id]
    | otherwise = [mkAbility a 4 $ forced $ PhaseBeginsWindow When EncounterPhase]

instance RunMessage PrancingPonyInvestigation where
  runMessage msg q@(PrancingPonyInvestigation a) = case msg of
    DoStep 7 Setup | toCardCode a == toCardCode Cards.investigatingTheCrime -> q <$ askToPerformSetup a
    PerformSetup (isTarget a -> True) | toCardCode a == toCardCode Cards.investigatingTheCrime -> do
      push $ ScenarioSpecific "pony.setup" Null
      turnOver a
      pure q
    Flip target | not a.flipped, isTarget a target || target == CardIdTarget a.cardId -> do
      when (toCardCode a /= toCardCode Cards.investigatingTheCrime) $ askToResolveWhenRevealed a
      PrancingPonyInvestigation <$> runMessage msg a
    WhenRevealed (isTarget a -> True) -> do
      if toCardCode a == toCardCode Cards.runningOutOfTime
        then do
          shuffleEncounterDiscardIntoDeck AnyCard
          doStep 1 msg
        else do
          lead <- getLead
          eachPlayer $ \pid -> when (pid /= lead) $ searchEncounterDeckAndDiscard pid a (CardWithTrait Brigand)
      pure q
    DoStep 1 (WhenRevealed (isTarget a -> True)) -> do
      players <- getRemainingPlayers
      discardEncounterCardsUntilCount a (length players) (CardWithType LocationType)
      pure q
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ filter (cardMatches $ CardWithType LocationType) cards
      pure q
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer $ \pid -> chooseOneM pid do
        questionLabeled "prancingPony.questChoice"
        causedBy a
        labeled "prancingPony.raiseThreat2" $ encounterRaisesThreat pid 2
        labeled "prancingPony.extraEncounter" $ phaseModifier a GameTarget (AdditionalStagingReveals 1)
      pure q
    UseCardAbility _ (isSource a -> True) 2 _ -> q <$ push (ScenarioSpecific "pony.loseClue" Null)
    UseCardAbility _ (isSource a -> True) 3 _ -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "prancingPony.accuseOrContinue"
        causedBy a
        labeled "prancingPony.continueInvestigating" revealEncounterCard
        labeled "prancingPony.accuse" $ push $ MakeAccusation lead Nothing Nothing
      pure q
    AccusationMade _ | toCardCode a == toCardCode Cards.runningOutOfTime -> q <$ push (CompleteQuest a.id)
    UseCardAbility _ (isSource a -> True) 4 _ -> do
      lead <- getLead
      select (EnemyWithTrait Suspect) >>= traverse_ (engageEnemy lead)
      pure q
    _ -> PrancingPonyInvestigation <$> runMessage msg a
