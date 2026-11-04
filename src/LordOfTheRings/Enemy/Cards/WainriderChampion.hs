module LordOfTheRings.Enemy.Cards.WainriderChampion (wainriderChampion) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers (atWainridersStage)

newtype WainriderChampion = WainriderChampion EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

wainriderChampion :: EnemyCard WainriderChampion
wainriderChampion = enemy WainriderChampion Cards.wainriderChampion

{- | X is 2 per player: its Threat and Defense. Indestructible.
Immune to player card effects. Cannot leave the staging area: no player engages it.
While The Challengers is at the same stage as The Wainriders, Wainrider Champion is
considered to be engaged with each player.
-}
instance HasModifiersFor WainriderChampion where
  getModifiersFor (WainriderChampion a) = do
    x <- (2 *) . length <$> getRemainingPlayers
    together <- atWainridersStage
    modifySelf a
      $ [ ImmuneToPlayerCardEffects
        , CannotLeaveStagingArea
        , NoEngagementChecks
        , CannotBeOptionallyEngagedBy AnyPlayer
        , ThreatModifier x
        , DefenseModifier x
        ]
      <> [EngagedWithEachPlayer AnyPlayer | together]

instance RunMessage WainriderChampion where
  runMessage msg (WainriderChampion attrs) = WainriderChampion <$> runMessage msg attrs
