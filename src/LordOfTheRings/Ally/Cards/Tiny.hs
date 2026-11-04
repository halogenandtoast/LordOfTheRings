module LordOfTheRings.Ally.Cards.Tiny (tiny) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (InPlayArea))

-- | Tiny, objective-ally side faceup (Roam Across Rhovanion): Rescue Tiny makes him the
-- captive of a Grey Moorland; flipped, he is the enemy of
-- "LordOfTheRings.Enemy.Cards.TinyEnemy".
newtype Tiny = Tiny AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

tiny :: AllyCard Tiny
tiny = ally Tiny Cards.tiny

-- | Immune to player card effects.
instance HasModifiersFor Tiny where
  getModifiersFor (Tiny a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: After Tiny exhausts, raise your threat by 2.
-- If Tiny is destroyed, the players lose the game.
instance HasAbilities Tiny where
  getAbilities (Tiny a) =
    [ mkAbility a 1 $ forced $ ExhaustedWindow After (toTarget a)
    , mkAbility a 2 $ forced $ TargetDestroyedWindow When (toTarget a)
    ]

-- | While Tiny is free of encounters, the first player gains control of Tiny: as he is
-- rescued, and as the first player token passes.
instance RunMessage Tiny where
  runMessage msg t@(Tiny a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      for_ [pid | InPlayArea pid <- [a.placement]] \pid -> raiseThreat pid 2
      pure t
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      gameOver Lost
      pure t
    RescueCharacter (isTarget a -> True) -> do
      lead <- getLead
      pure $ Tiny $ a & placementL .~ InPlayArea lead
    SetFirstPlayer pid | InPlayArea _ <- a.placement -> pure $ Tiny $ a & placementL .~ InPlayArea pid
    _ -> Tiny <$> runMessage msg a
