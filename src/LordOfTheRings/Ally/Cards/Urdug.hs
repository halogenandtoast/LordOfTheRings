module LordOfTheRings.Ally.Cards.Urdug (urdug) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (InPlayArea))

-- | Urdug, objective-ally side faceup (Roam Across Rhovanion). Guarded, as Sneaking Off
-- attaches him to a location, he is that location's captive; flipped, he is the enemy of
-- "LordOfTheRings.Enemy.Cards.UrdugEnemy".
newtype Urdug = Urdug AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

urdug :: AllyCard Urdug
urdug = ally Urdug Cards.urdug

-- | Immune to player card effects.
instance HasModifiersFor Urdug where
  getModifiersFor (Urdug a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | If Urdug is destroyed, the players lose the game.
instance HasAbilities Urdug where
  getAbilities (Urdug a) = [mkAbility a 1 $ forced $ TargetDestroyedWindow When (toTarget a)]

-- | While Urdug is free of encounters, the first player gains control of Urdug: as he is
-- rescued, and as the first player token passes.
instance RunMessage Urdug where
  runMessage msg u@(Urdug a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      gameOver Lost
      pure u
    RescueCharacter (isTarget a -> True) -> do
      lead <- getLead
      pure $ Urdug $ a & placementL .~ InPlayArea lead
    SetFirstPlayer pid | InPlayArea _ <- a.placement -> pure $ Urdug $ a & placementL .~ InPlayArea pid
    _ -> Urdug <$> runMessage msg a
