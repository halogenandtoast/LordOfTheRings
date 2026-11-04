module LordOfTheRings.Enemy.Cards.TheMouthOfSauron (theMouthOfSauron) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype TheMouthOfSauron = TheMouthOfSauron EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

theMouthOfSauron :: EnemyCard TheMouthOfSauron
theMouthOfSauron = enemy TheMouthOfSauron Cards.theMouthOfSauron

-- | Immune to player card effects. Cannot leave the staging area.
instance HasModifiersFor TheMouthOfSauron where
  getModifiersFor (TheMouthOfSauron a) = modifySelf a [ImmuneToPlayerCardEffects, CannotLeaveStagingArea]

-- | Forced: After a player cancels a 'when revealed' effect, The Mouth of Sauron makes an
-- immediate attack against that player.
instance HasAbilities TheMouthOfSauron where
  getAbilities (TheMouthOfSauron a) = [mkAbility a 1 $ forced $ WhenRevealedCanceledWindow After]

instance RunMessage TheMouthOfSauron where
  runMessage msg e@(TheMouthOfSauron attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (WhenRevealedCanceled pid _) <- ws] \pid -> immediateAttack pid attrs.id
      pure e
    _ -> TheMouthOfSauron <$> runMessage msg attrs
