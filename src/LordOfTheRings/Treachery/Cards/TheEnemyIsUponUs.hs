module LordOfTheRings.Treachery.Cards.TheEnemyIsUponUs (theEnemyIsUponUs) where

import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheEnemyIsUponUs = TheEnemyIsUponUs TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theEnemyIsUponUs :: TreacheryCard TheEnemyIsUponUs
theEnemyIsUponUs = treachery TheEnemyIsUponUs Cards.theEnemyIsUponUs

-- | When Revealed: Each engaged enemy makes an immediate attack. If no attack is made this
-- way, The Enemy is Upon Us! gains surge.
-- Shadow: If this attack destroys a character, remove all progress from the current quest.
instance RunMessage TheEnemyIsUponUs where
  runMessage msg t@(TheEnemyIsUponUs attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- traverse getEnemy =<< select AnyEnemy
      let engaged = [(p, toId e) | e <- enemies, EngagedWith p <- [e.placement]]
      if null engaged
        then revealEncounterCard
        else for_ engaged \(p, eid) -> immediateAttack p eid
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      removeProgressFromQuest attrs maxBound
      pure t
    _ -> TheEnemyIsUponUs <$> runMessage msg attrs
