module LordOfTheRings.Attachment.Cards.PoisonedStakes (poisonedStakes) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype PoisonedStakes = PoisonedStakes AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Play Poisoned Stakes into the staging area unattached. If unattached, attach Poisoned
-- Stakes to the next eligible enemy that enters the staging area.
poisonedStakes :: AttachmentCard PoisonedStakes
poisonedStakes = attachment PoisonedStakes Cards.poisonedStakes & attachTo PlayIntoStagingArea

-- | At the end of each round, deal 2 damage to attached enemy.
instance HasAbilities PoisonedStakes where
  getAbilities (PoisonedStakes a) = [mkAbility a 1 $ forced RoundEndsWindow | AttachedTo _ <- [a.placement]]

instance RunMessage PoisonedStakes where
  runMessage msg s@(PoisonedStakes a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> dealDamage a host 2
      pure s
    _ -> PoisonedStakes <$> runMessage msg a
