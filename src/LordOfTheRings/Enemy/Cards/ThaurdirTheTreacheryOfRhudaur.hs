module LordOfTheRings.Enemy.Cards.ThaurdirTheTreacheryOfRhudaur (thaurdirTheTreacheryOfRhudaur) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Sorcery))

newtype ThaurdirTheTreacheryOfRhudaur = ThaurdirTheTreacheryOfRhudaur EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Indestructible, as its card def says.
thaurdirTheTreacheryOfRhudaur :: EnemyCard ThaurdirTheTreacheryOfRhudaur
thaurdirTheTreacheryOfRhudaur = enemy ThaurdirTheTreacheryOfRhudaur Cards.thaurdirTheTreacheryOfRhudaur

-- | Cannot have attachments.
instance HasModifiersFor ThaurdirTheTreacheryOfRhudaur where
  getModifiersFor (ThaurdirTheTreacheryOfRhudaur a) = modified a a [CannotHaveAttachments]

-- | Forced: After a treachery card with the Sorcery trait is revealed from the encounter
-- deck, ThaurdirTheTreacheryOfRhudaur heals 3 damage and makes an immediate attack against the first player.
instance HasAbilities ThaurdirTheTreacheryOfRhudaur where
  getAbilities (ThaurdirTheTreacheryOfRhudaur a) = [mkAbility a 1 $ forced $ TreacheryWithTraitRevealedWindow After Sorcery]

instance RunMessage ThaurdirTheTreacheryOfRhudaur where
  runMessage msg e@(ThaurdirTheTreacheryOfRhudaur attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      healDamage attrs attrs 3
      lead <- getLead
      immediateAttack lead attrs.id
      pure e
    _ -> ThaurdirTheTreacheryOfRhudaur <$> runMessage msg attrs
