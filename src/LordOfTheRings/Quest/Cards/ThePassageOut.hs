module LordOfTheRings.Quest.Cards.ThePassageOut (thePassageOut) where

import LordOfTheRings.Card (HasCardCode, toCardCode)
import LordOfTheRings.Entities (entitiesEnemies)
import LordOfTheRings.Scenario.Types (scenarioSetAside)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (enemyTokens)
import LordOfTheRings.Helpers.Staging (getEnemyHitPoints)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ThePassageOut = ThePassageOut QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

thePassageOut :: QuestCard ThePassageOut
thePassageOut = quest ThePassageOut Cards.thePassageOut

-- | Siege. Characters use their willpower instead of attack when attacking enemies. If an
-- enemy would be damaged this way, place progress tokens on it instead of damage tokens.
-- Drû-buri-Drû's: while he is in the victory display, characters get +1 willpower and +1
-- defense.
instance HasModifiersFor ThePassageOut where
  getModifiersFor (ThePassageOut a) = do
    modified a GameTarget [WillpowerForAttack, ProgressInsteadOfAttackDamage]
    whenM druInVictoryDisplay do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> modified a c [WillpowerModifier 1, DefenseModifier 1]

-- | When Revealed: Add Drû-buri-Drû to the staging area.
-- When an enemy has progress equal to its hit points, add it to the victory display and
-- place those progress tokens on the quest.
-- If the players defeat this stage, they have won the game: unless Drû-buri-Drû is in the
-- victory display, they cannot win, so its progress piles up until he is.
instance RunMessage ThePassageOut where
  runMessage msg q@(ThePassageOut attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      stage
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.druBuriDru)
      pure q
    -- the enemy counts its progress once it has taken it
    PlaceTokens _ (EnemyTarget _) Progress _ -> do
      doStep 1 msg
      stage
    DoStep 1 (PlaceTokens _ (EnemyTarget eid) Progress _) -> do
      whenM (selectAny $ EnemyWithId eid) do
        progress <- countTokens Progress . enemyTokens . toAttrs <$> getEnemy eid
        hitPoints <- getEnemyHitPoints eid
        when (progress >= hitPoints) do
          addToVictoryDisplay eid
          placeProgress attrs attrs progress
      pure q
    _ -> stage
   where
    stage = ThePassageOut <$> runConditionalStage mayWin msg attrs

{- | His "cannot win" holds only while he is in play: in the victory display, or destroyed
by damage and discarded, he no longer stops it (ruling); set aside, he is still to come.
-}
mayWin :: HasGame m => m Bool
mayWin = do
  let dru :: HasCardCode c => c -> Bool
      dru c = toCardCode c == toCardCode Enemies.druBuriDru
  inPlay <- getsGame (any dru . toList . entitiesEnemies . (.entities))
  aside <- getsGame (any dru . scenarioSetAside . toAttrs . (.scenario))
  pure $ not inPlay && not aside

druInVictoryDisplay :: HasGame m => m Bool
druInVictoryDisplay =
  getsGame (any ((== toCardCode Enemies.druBuriDru) . toCardCode) . (.victoryDisplay) . toAttrs . (.scenario))
