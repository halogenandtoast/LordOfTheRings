module LordOfTheRings.Enemy.Cards.DaughterOfBeruthiel (daughterOfBeruthiel) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea), placementController)

newtype DaughterOfBeruthiel = DaughterOfBeruthiel EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

daughterOfBeruthiel :: EnemyCard DaughterOfBeruthiel
daughterOfBeruthiel = enemy DaughterOfBeruthiel Cards.daughterOfBeruthiel

-- | Daughter of Berúthiel cannot be optionally engaged.
instance HasModifiersFor DaughterOfBeruthiel where
  getModifiersFor (DaughterOfBeruthiel a) = modified a a [CannotBeOptionallyEngagedBy AnyPlayer]

-- | Forced: After Daughter of Berúthiel attacks, return her to the staging area. The
-- engaged player may raise his threat by 4 to prevent this effect.
instance HasAbilities DaughterOfBeruthiel where
  getAbilities (DaughterOfBeruthiel a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage DaughterOfBeruthiel where
  runMessage msg e@(DaughterOfBeruthiel attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      case placementController attrs.placement of
        Just pid -> chooseOneM pid do
          questionLabeled "daughterOfBeruthiel.prevent"
          causedBy attrs
          labeled "daughterOfBeruthiel.raiseThreat" $ raiseThreat pid 4
          labeled "daughterOfBeruthiel.return" $ placeEnemy attrs.id StagingArea
        Nothing -> pure ()
      pure e
    _ -> DaughterOfBeruthiel <$> runMessage msg attrs
