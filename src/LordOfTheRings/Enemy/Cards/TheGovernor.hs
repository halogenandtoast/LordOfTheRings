module LordOfTheRings.Enemy.Cards.TheGovernor (theGovernor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype TheGovernor = TheGovernor EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

theGovernor :: EnemyCard TheGovernor
theGovernor = enemy TheGovernor Cards.theGovernor

-- | Immune to player card effects.
instance HasModifiersFor TheGovernor where
  getModifiersFor (TheGovernor a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: After a location is revealed from the encounter deck, The Governor engages the
-- first player and makes an immediate attack.
instance HasAbilities TheGovernor where
  getAbilities (TheGovernor a) = [mkAbility a 1 $ forced $ LocationRevealedWindow After]

instance RunMessage TheGovernor where
  runMessage msg e@(TheGovernor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      engageEnemy lead attrs.id
      immediateAttack lead attrs.id
      pure e
    _ -> TheGovernor <$> runMessage msg attrs
