module LordOfTheRings.Quest.Cards.TheGreatWorm (theGreatWorm) where

import LordOfTheRings.Card (CardType (QuestType), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers
import LordOfTheRings.Trait (Trait (Reforged))

newtype TheGreatWorm = TheGreatWorm QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theGreatWorm :: QuestCard TheGreatWorm
theGreatWorm = quest TheGreatWorm Cards.theGreatWorm

-- | Dagnir gets -5 engagement cost for each character the first player controls.
instance HasModifiersFor TheGreatWorm where
  getModifiersFor (TheGreatWorm a) = when a.flipped do
    characters <- charactersControlledBy =<< getLead
    getDagnir >>= traverse_ \dagnir -> modified a dagnir [EngagementCostModifier (-5 * length characters)]

{- | Response: After a player resolves an attack against Dagnir, if Dagnir has no hit points
remaining, that player may discard a Reforged Wormsbane he controls to add Dagnir to the
victory display. Then, the players win the game.
At the end of the combat phase, if there are 4 or fewer quests in the victory display, the
players must bypass this stage; with more, they may.
-}
instance HasAbilities TheGreatWorm where
  getAbilities (TheGreatWorm a) =
    exploringGundabad a
      <> [ ability
         | a.flipped
         , ability <-
             [ anyPlayer
                 $ restricted
                   a
                   1
                   ( exists (dagnir <> EnemyWithNoHitPointsRemaining)
                       <> exists (HeroControlledBy ActivePlayer <> HeroWithAttached Reforged)
                   )
                 $ response
                 $ MatchingEnemyAttackResolvedWindow After dagnir
             , restricted a 102 (FewerInVictoryDisplay 5 quests) $ forced $ PhaseEndsWindow When CombatPhase
             , restricted a 102 (NotCriterion $ FewerInVictoryDisplay 5 quests) $ response $ PhaseEndsWindow When CombatPhase
             ]
         ]
   where
    dagnir = EnemyWithCardCode (toCardCode Enemies.dagnirMountGundabad)
    quests = CardWithType QuestType

-- | Only the player whose attack it was may discard his Wormsbane.
instance RunMessage TheGreatWorm where
  runMessage msg q@(TheGreatWorm attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 ws -> do
      attacking <- catMaybes <$> traverse characterController [c | Window _ (AttackedEnemy c _) <- ws]
      wielding <- selectAny (HeroControlledBy (PlayerWithId pid) <> HeroWithAttached Reforged)
      when (pid `elem` attacking && wielding) do
        getWormsbane >>= traverse_ (discard . ObjectiveTarget)
        getDagnir >>= traverse_ (addToVictoryDisplay . EnemyTarget)
        winGame
      pure q
    _ -> TheGreatWorm <$> runExploringGundabad (pure ()) msg attrs
