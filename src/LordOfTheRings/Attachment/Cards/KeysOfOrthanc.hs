module LordOfTheRings.Attachment.Cards.KeysOfOrthanc (keysOfOrthanc) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype KeysOfOrthanc = KeysOfOrthanc AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero.
keysOfOrthanc :: AttachmentCard KeysOfOrthanc
keysOfOrthanc = attachment KeysOfOrthanc Cards.keysOfOrthanc & attachTo (AttachToHero AnyHero)

-- | Response: After you raise your threat from the Doomed keyword, exhaust Keys of Orthanc
-- to add 1 resource to attached hero's resource pool.
instance HasAbilities KeysOfOrthanc where
  getAbilities (KeysOfOrthanc a) =
    [ mkAbility a 1 $ ResponseAbility (ThreatRaisedByDoomedWindow After a.controller) (exhaustCost a)
    | AttachedTo _ <- [a.placement]
    ]

instance RunMessage KeysOfOrthanc where
  runMessage msg s@(KeysOfOrthanc a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> placeResources a host 1
      pure s
    _ -> KeysOfOrthanc <$> runMessage msg a
