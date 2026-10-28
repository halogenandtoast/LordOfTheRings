module LordOfTheRings.Enemy.Cards.DruadanDrummer (druadanDrummer) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Wose))

newtype DruadanDrummer = DruadanDrummer EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

druadanDrummer :: EnemyCard DruadanDrummer
druadanDrummer = enemy DruadanDrummer Cards.druadanDrummer

-- | Each Wose enemy in the staging area gets +2 threat.
instance HasModifiersFor DruadanDrummer where
  getModifiersFor (DruadanDrummer a) = do
    woses <- select (EnemyWithTrait Wose <> EnemyInStagingArea)
    for_ woses \w -> modified a w [ThreatModifier 2]

-- | Shadow: Each Wose enemy engaged with the defending player gets +1 attack.
instance RunMessage DruadanDrummer where
  runMessage msg e@(DruadanDrummer attrs) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      woses <- select (EnemyWithTrait Wose <> EnemyEngagedWith (PlayerWithId pid))
      for_ woses \w -> phaseModifier attrs w (AttackModifier 1)
      pure e
    _ -> DruadanDrummer <$> runMessage msg attrs
