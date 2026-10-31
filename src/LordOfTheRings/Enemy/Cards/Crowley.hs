module LordOfTheRings.Enemy.Cards.Crowley (crowley) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))

newtype Crowley = Crowley EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Dream-chaser campaign, shuffled into the Corsair deck.
crowley :: EnemyCard Crowley
crowley = enemy Crowley Cards.crowley

resources :: EnemyAttrs -> Int
resources = countTokens Resource . enemyTokens

-- | For each resource on Crowley, he gets +1 threat, +1 attack, +1 defense, and +1 hit
-- points. If Crowley has 3 or more resources on him, he gains victory 3: defeated, he
-- stays for his Forced effect to add him to the victory display.
instance HasModifiersFor Crowley where
  getModifiersFor (Crowley a) = do
    let n = resources a
    when (n > 0) $ modifySelf a [ThreatModifier n, AttackModifier n, DefenseModifier n, HitPointModifier n]
    when (n >= 3) $ modifySelf a [StaysInPlayWhenDefeated]

-- | Forced: When Crowley engages you, place 1 resource on him. At the end of the combat
-- phase, return him to the staging area.
instance HasAbilities Crowley where
  getAbilities (Crowley a) =
    [ mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id
    , mkAbility a 2 $ forced $ PhaseEndsWindow After CombatPhase
    ]
      <> [mkAbility a 3 $ silent $ ThisEnemyDefeatedWindow After a.id | resources a >= 3]

instance RunMessage Crowley where
  runMessage msg e@(Crowley attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      case attrs.placement of
        EngagedWith _ -> placeEnemy attrs.id StagingArea
        _ -> pure ()
      pure e
    UseCardAbility _ (isSource attrs -> True) 3 _ -> do
      addToVictoryDisplay attrs
      pure e
    _ -> Crowley <$> runMessage msg attrs
