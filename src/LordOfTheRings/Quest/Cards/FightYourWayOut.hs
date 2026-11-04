module LordOfTheRings.Quest.Cards.FightYourWayOut (fightYourWayOut) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDungeonsOfCirithGurat.Helpers (captiveOfStaged, guardingAnObjective, unattachedInStaging)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))

newtype FightYourWayOut = FightYourWayOut QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

fightYourWayOut :: QuestCard FightYourWayOut
fightYourWayOut = quest FightYourWayOut Cards.fightYourWayOut

-- | The players cannot defeat this stage while a guarded objective is in play. If the
-- players defeat this stage, they win the game: it is the last stage.
instance HasModifiersFor FightYourWayOut where
  getModifiersFor (FightYourWayOut a) = when a.flipped do
    captives <- guardedCharacters
    when (notNull captives) $ modifySelf a [CannotBeDefeated]

-- | Forced: At the beginning of the quest phase, discard cards from the encounter deck
-- until an enemy is discarded and add that enemy to the staging area.
instance HasAbilities FightYourWayOut where
  getAbilities (FightYourWayOut a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase | a.flipped]

{- | When Revealed: The first player adds Uruk Chieftain to the staging area. Each player
chooses a hero he controls, discards all tokens and attachments from it, and attaches it to
an unattached enemy in the staging area as a captive. Then, the players make a standard
series of engagement checks against each enemy in the staging area. Reduce the engagement
cost of each enemy not guarding an objective to 0 during these engagement checks.

The Captives of Cirith Gurat discards the hero's tokens and attachments as it is attached.
Player by player, step N the Nth player's hero; step N+1 the engagement checks.
-}
instance RunMessage FightYourWayOut where
  runMessage msg q@(FightYourWayOut attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      FightYourWayOut <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      traverse_ addToStaging $ take 1 [c | c <- aside, toCardCode c == toCardCode Enemies.urukChieftain]
      doStep 1 msg
      pure q
    DoStep n inner@(WhenRevealed (isTarget attrs -> True)) -> do
      players <- getPlayerOrder
      case drop (n - 1) players of
        (p : _) -> do
          heroes <- select (HeroControlledBy $ PlayerWithId p)
          captors <- unattachedInStaging False
          unless (null captors) $ chooseOrRunOneM p do
            questionLabeled "fightYourWayOut.hero"
            causedBy attrs
            targets heroes \h -> chooseOrRunOneM p do
              questionLabeled "theDungeonsOfCirithGurat.attachCaptive"
              causedBy attrs
              targets captors (captiveOfStaged h)
          doStep (n + 1) inner
        [] -> do
          staged <- select EnemyInStagingArea
          lowered <- fmap catMaybes $ for staged \e -> do
            guarding <- guardingAnObjective =<< getEnemy e
            cost <- getEngagementCost e
            pure $ guard (not guarding && cost > 0) $> Effect (toSource attrs) (toTarget e) [EngagementCostModifier (negate cost)] UntilEndOfEngagementChecks
          makeEngagementChecksWith lowered
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardEncounterCardsUntil attrs (CardWithType EnemyType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (take 1 $ filter (cardMatches $ CardWithType EnemyType) cards) addToStagingFromDiscard
      pure q
    _ -> FightYourWayOut <$> runMessage msg attrs
