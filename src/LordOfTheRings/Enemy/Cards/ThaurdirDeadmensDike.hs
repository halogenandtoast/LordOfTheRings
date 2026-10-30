module LordOfTheRings.Enemy.Cards.ThaurdirDeadmensDike (thaurdirDeadmensDike) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Sorcery))

newtype ThaurdirDeadmensDike = ThaurdirDeadmensDike EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

thaurdirDeadmensDike :: EnemyCard ThaurdirDeadmensDike
thaurdirDeadmensDike = enemy ThaurdirDeadmensDike Cards.thaurdirDeadmensDike

-- | Indestructible. Cannot have attachments.
instance HasModifiersFor ThaurdirDeadmensDike where
  getModifiersFor (ThaurdirDeadmensDike a) = modified a a [CannotHaveAttachments]

-- | Forced: After a treachery card with the Sorcery trait is revealed from the encounter
-- deck, ThaurdirDeadmensDike heals 3 damage and makes an immediate attack against the first player.
instance HasAbilities ThaurdirDeadmensDike where
  getAbilities (ThaurdirDeadmensDike a) = [mkAbility a 1 $ forced $ TreacheryWithTraitRevealedWindow After Sorcery]

instance RunMessage ThaurdirDeadmensDike where
  runMessage msg e@(ThaurdirDeadmensDike attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      healDamage attrs attrs 3
      lead <- getLead
      immediateAttack lead attrs.id
      pure e
    _ -> ThaurdirDeadmensDike <$> runMessage msg attrs
