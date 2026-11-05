module LordOfTheRings.Attachment.Cards.Ambush (ambush) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype Ambush = Ambush AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Play Ambush into the staging area unattached. If unattached, attach Ambush to the next
-- eligible enemy that enters the staging area.
ambush :: AttachmentCard Ambush
ambush = attachment Ambush Cards.ambush & attachTo PlayIntoStagingArea

-- | Combat Action: The engaged player discards Ambush to declare an attack against the
-- attached enemy.
instance HasAbilities Ambush where
  getAbilities (Ambush a) =
    [ anyPlayer
        $ restricted a 1 (DuringPhase CombatPhase <> exists (EnemyWithId eid <> EnemyEngagedWith ActivePlayer))
        $ ActionAbility Free
    | AttachedTo (EnemyTarget eid) <- [a.placement]
    ]

instance RunMessage Ambush where
  runMessage msg s@(Ambush a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      withHost a \case
        EnemyTarget eid -> do
          discard a a
          push $ DeclareAttackers pid eid
        _ -> pure ()
      pure s
    _ -> Ambush <$> runMessage msg a
