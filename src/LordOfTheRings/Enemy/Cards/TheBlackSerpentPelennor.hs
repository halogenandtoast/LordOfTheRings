module LordOfTheRings.Enemy.Cards.TheBlackSerpentPelennor (theBlackSerpentPelennor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype TheBlackSerpentPelennor = TheBlackSerpentPelennor EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Black Serpent of The Battle of the Pelennor Fields.
theBlackSerpentPelennor :: EnemyCard TheBlackSerpentPelennor
theBlackSerpentPelennor = enemy TheBlackSerpentPelennor Cards.theBlackSerpentPelennor

-- | Cannot have attachments or be attacked by more than 1 character at a time.
instance HasModifiersFor TheBlackSerpentPelennor where
  getModifiersFor (TheBlackSerpentPelennor a) = modifySelf a [CannotHaveAttachments, AttackersPerAttackLimit 1]

-- | When Revealed: The Black Serpent makes an immediate attack against the first player.
instance RunMessage TheBlackSerpentPelennor where
  runMessage msg e@(TheBlackSerpentPelennor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      immediateAttack lead attrs.id
      pure e
    _ -> TheBlackSerpentPelennor <$> runMessage msg attrs
