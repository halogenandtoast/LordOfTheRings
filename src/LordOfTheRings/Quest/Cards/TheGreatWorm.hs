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
                   (exists (dagnir <> EnemyWithNoHitPointsRemaining))
                 $ response
                 $ MatchingEnemyAttackResolvedWindow After (HeroControlledBy ActivePlayer <> HeroWithAttached Reforged) dagnir
             , restricted a 102 (FewerInVictoryDisplay 5 quests) $ forced $ PhaseEndsWindow When CombatPhase
             , restricted a 102 (NotCriterion $ FewerInVictoryDisplay 5 quests) $ response $ PhaseEndsWindow When CombatPhase
             ]
         ]
   where
    dagnir = EnemyWithCardCode (toCardCode Enemies.dagnirMountGundabad)
    quests = CardWithType QuestType

{- | Offered only to the player whose attack it was, and only with his hero wielding Reforged
Wormsbane among its attackers: the window reads the attack's attackers as that player's.
Dagnir goes to the victory display though with 5 quest cards there she "cannot leave the
staging area": the card names her and the move, and the win follows from it ("Then, the
players win the game"; FAQ 1.15, "then" needs what precedes it to resolve).
-}
instance RunMessage TheGreatWorm where
  runMessage msg q@(TheGreatWorm attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      getWormsbane >>= traverse_ (discard . ObjectiveTarget)
      getDagnir >>= traverse_ (addToVictoryDisplayOverriding . EnemyTarget)
      winGame
      pure q
    _ -> TheGreatWorm <$> runExploringGundabad (pure ()) msg attrs
