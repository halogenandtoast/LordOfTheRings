module LordOfTheRings.Enemy.Cards.KhamulTheEasterlingNightmare (khamulTheEasterlingNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Fellowship (theOneRingExhausted)
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Trait (Trait (Morgul))

newtype KhamulTheEasterlingNightmare = KhamulTheEasterlingNightmare EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

khamulTheEasterlingNightmare :: EnemyCard KhamulTheEasterlingNightmare
khamulTheEasterlingNightmare = enemy KhamulTheEasterlingNightmare Cards.khamulTheEasterlingNightmare

instance HasModifiersFor KhamulTheEasterlingNightmare where
  getModifiersFor (KhamulTheEasterlingNightmare a) = do
    exhausted <- theOneRingExhausted
    modifySelf a $ CannotHaveAttachmentsWithoutTrait Morgul : [EngagementCostModifier (-30) | exhausted]

instance HasAbilities KhamulTheEasterlingNightmare where
  getAbilities (KhamulTheEasterlingNightmare a) = case a.placement of
    EngagedWith _ -> [mkAbility a 1 $ forced $ PhaseBeginsWindow After RefreshPhase]
    _ -> []

instance RunMessage KhamulTheEasterlingNightmare where
  runMessage msg e@(KhamulTheEasterlingNightmare a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      case a.placement of
        EngagedWith pid -> hideTest pid a 2
        _ -> pure ()
      pure e
    HideTestResult test False | test.hideSource == toSource a -> do
      immediateAttack test.hidePlayer a.id
      pure e
    _ -> KhamulTheEasterlingNightmare <$> runMessage msg a
