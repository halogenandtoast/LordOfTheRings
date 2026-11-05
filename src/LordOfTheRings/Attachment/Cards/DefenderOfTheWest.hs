module LordOfTheRings.Attachment.Cards.DefenderOfTheWest (defenderOfTheWest) where

import LordOfTheRings.Ally.Types (AllyAttrs (allyOwner))
import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype DefenderOfTheWest = DefenderOfTheWest AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a non-objective unique ally in play.
defenderOfTheWest :: AttachmentCard DefenderOfTheWest
defenderOfTheWest =
  attachment DefenderOfTheWest Cards.defenderOfTheWest & attachTo (AttachToAlly $ UniqueAlly <> not_ ObjectiveAlly)

-- | Damage from undefended attacks against you may be assigned to attached ally.
-- "You" is the player controlling Defender of the West, who played it, whoever controls the
-- ally.
instance HasModifiersFor DefenderOfTheWest where
  getModifiersFor (DefenderOfTheWest a) = case a.placement of
    AttachedTo host@(AllyTarget _) -> modified a (PlayerTarget a.controller) [UndefendedDamageMayGoTo host]
    _ -> pure ()

-- | The first player gains control of attached ally, as long as it is attached: as it
-- attaches, as the first player token passes, and back to its owner as it leaves.
instance RunMessage DefenderOfTheWest where
  runMessage msg (DefenderOfTheWest a) = case msg of
    AttachCard aid (AllyTarget ally) | aid == a.id -> do
      takeControl ally =<< getLead
      DefenderOfTheWest <$> runMessage msg a
    SetFirstPlayer pid | AttachedTo (AllyTarget ally) <- a.placement -> do
      takeControl ally pid
      DefenderOfTheWest <$> runMessage msg a
    Discard _ (isTarget a -> True) | AttachedTo (AllyTarget ally) <- a.placement -> do
      owner <- allyOwner . toAttrs <$> getAlly ally
      takeControl ally owner
      DefenderOfTheWest <$> runMessage msg a
    _ -> DefenderOfTheWest <$> runMessage msg a
