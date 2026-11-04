module LordOfTheRings.Enemy.Cards.FireDrake (fireDrake) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype FireDrake = FireDrake EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

fireDrake :: EnemyCard FireDrake
fireDrake = enemy FireDrake Cards.fireDrake

{- | Cannot have non-Dragon attachments. X is 1 more than the number of players: its
threat and defense. No player attachment has the Dragon trait, so none can be attached to
it; the encounter cards that attach to it, Dragon Might's, are Dragons.
-}
instance HasModifiersFor FireDrake where
  getModifiersFor (FireDrake a) = do
    x <- (+ 1) . length <$> getRemainingPlayers
    modifySelf a [CannotHavePlayerAttachments, ThreatModifier x, DefenseModifier x]

-- | Forced: After the active location is explored, Fire-drake returns to the staging area
-- and makes an immediate attack against each player in turn order.
instance HasAbilities FireDrake where
  getAbilities (FireDrake a) = [mkAbility a 1 $ forced $ ActiveLocationExploredWindow After]

instance RunMessage FireDrake where
  runMessage msg e@(FireDrake attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      when (attrs.placement /= StagingArea) $ placeEnemy attrs.id StagingArea
      eachPlayer (`immediateAttack` attrs.id)
      pure e
    _ -> FireDrake <$> runMessage msg attrs
