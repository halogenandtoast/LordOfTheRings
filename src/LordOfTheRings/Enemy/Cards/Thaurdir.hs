module LordOfTheRings.Enemy.Cards.Thaurdir (thaurdir) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Sorcery))

newtype Thaurdir = Thaurdir EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

thaurdir :: EnemyCard Thaurdir
thaurdir = enemy Thaurdir Cards.thaurdir

-- | Indestructible. Cannot have attachments.
instance HasModifiersFor Thaurdir where
  getModifiersFor (Thaurdir a) = modified a a [CannotHaveAttachments]

-- | Forced: After a treachery card with the Sorcery trait is revealed from the encounter
-- deck, Thaurdir heals 3 damage and makes an immediate attack against the first player.
instance HasAbilities Thaurdir where
  getAbilities (Thaurdir a) = [mkAbility a 1 $ forced $ TreacheryWithTraitRevealedWindow After Sorcery]

instance RunMessage Thaurdir where
  runMessage msg e@(Thaurdir attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      healDamage attrs attrs 3
      lead <- getLead
      immediateAttack lead attrs.id
      pure e
    _ -> Thaurdir <$> runMessage msg attrs
