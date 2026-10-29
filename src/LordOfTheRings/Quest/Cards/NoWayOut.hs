module LordOfTheRings.Quest.Cards.NoWayOut (noWayOut) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Keyword (Keyword (Time))
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))

newtype NoWayOut = NoWayOut QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

noWayOut :: QuestCard NoWayOut
noWayOut = quest NoWayOut Cards.noWayOut

-- | Time X. X is 5 times the number of players in the game.
instance HasModifiersFor NoWayOut where
  getModifiersFor (NoWayOut a) = do
    players <- length <$> getRemainingPlayers
    modified a a [GainKeyword (Time $ 5 * players)]

-- | Forced: After the last time counter is removed from this stage, the players make
-- engagement checks. Then, each engaged enemy makes an attack. If no heroes are destroyed
-- by these attacks, the players win the game.
instance HasAbilities NoWayOut where
  getAbilities (NoWayOut a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | When Revealed: Put Chief Turch into play, engaged with the first player.
-- If a hero is destroyed at this stage, the players lose the game: so the attacks end in a
-- win only if no hero was destroyed by them.
instance RunMessage NoWayOut where
  runMessage msg q@(NoWayOut attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      NoWayOut <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      traverse_ (putEnemyIntoPlayEngaged lead) $ filter (cardMatches $ cardIs Enemies.chiefTurchTheDunlandTrap) aside
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      makeEngagementChecks
      doStep 1 msg
      pure q
    -- each enemy attacks the player it is engaged with, in turn order, dealt a shadow card
    -- as an attack outside the combat phase is
    -- ponytail: a player's engaged enemies attack in the order they are found, not in an
    -- order he picks; nothing in the scenario depends on that order.
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      eachPlayer \p -> traverse_ (immediateAttack p) =<< select (EnemyEngagedWith $ PlayerWithId p)
      winGame
      pure q
    Destroy (HeroTarget _) -> do
      gameOver Lost
      NoWayOut <$> runMessage msg attrs
    _ -> NoWayOut <$> runMessage msg attrs
