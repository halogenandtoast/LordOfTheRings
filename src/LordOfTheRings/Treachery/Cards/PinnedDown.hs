module LordOfTheRings.Treachery.Cards.PinnedDown (pinnedDown) where

import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PinnedDown = PinnedDown TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pinnedDown :: TreacheryCard PinnedDown
pinnedDown = treachery PinnedDown Cards.pinnedDown

-- | When Revealed: Add X to the archery total this round. X is the number of locations the
-- players control. If the players control less than 4 locations, Pinned Down gains surge.
-- Shadow: If this attack destroys a character, that character's controller must return a
-- location he controls with the highest threat to the staging area.
instance RunMessage PinnedDown where
  runMessage msg t@(PinnedDown attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- length <$> select (LocationControlledBy AnyPlayer)
      when (x > 0) $ roundModifier attrs GameTarget (ArcheryBonus x)
      when (x < 4) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      destroyedReturnLocations attrs eid
      pure t
    _ -> PinnedDown <$> runMessage msg attrs
