module LordOfTheRings.Enemy.Cards.AngmarRaider (angmarRaider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype AngmarRaider = AngmarRaider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

angmarRaider :: EnemyCard AngmarRaider
angmarRaider = enemy AngmarRaider Cards.angmarRaider

-- | Forced: After a character the engaged player controls is healed, Angmar Raider makes
-- an immediate attack.
instance HasAbilities AngmarRaider where
  getAbilities (AngmarRaider a) = [mkAbility a 1 $ forced $ HealedByOtherWindow After (toSource a) | EngagedWith _ <- [a.placement]]

-- | Shadow: Attacking enemy gets +2 defense until the end of the phase.
instance RunMessage AngmarRaider where
  runMessage msg e@(AngmarRaider attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | EngagedWith pid <- [attrs.placement]] \pid -> do
        healed <- traverse characterController [t | Window _ (Healed t _) <- ws]
        when (Just pid `elem` healed) $ immediateAttack pid attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      phaseModifier attrs eid (DefenseModifier 2)
      pure e
    _ -> AngmarRaider <$> runMessage msg attrs
