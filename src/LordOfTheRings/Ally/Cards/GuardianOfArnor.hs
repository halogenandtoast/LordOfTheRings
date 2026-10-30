module LordOfTheRings.Ally.Cards.GuardianOfArnor (guardianOfArnor) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype GuardianOfArnor = GuardianOfArnor AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel, on its card def.
guardianOfArnor :: AllyCard GuardianOfArnor
guardianOfArnor = ally GuardianOfArnor Cards.guardianOfArnor

-- | Guardian of Arnor gets +1 defense for each enemy engaged with you.
instance HasModifiersFor GuardianOfArnor where
  getModifiersFor (GuardianOfArnor a) = for_ (placementController a.placement) \pid -> do
    engaged <- length <$> select (EnemyEngagedWith $ PlayerWithId pid)
    modifySelf a [DefenseModifier engaged | engaged > 0]
