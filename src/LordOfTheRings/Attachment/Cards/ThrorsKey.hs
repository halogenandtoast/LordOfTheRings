module LordOfTheRings.Attachment.Cards.ThrorsKey (throrsKey) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ThrorsKey = ThrorsKey AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero.
throrsKey :: AttachmentCard ThrorsKey
throrsKey = attachment ThrorsKey Cards.throrsKey

-- | While attached to a location, Thrór's Key gains: "Treat attached location's printed text
-- box as blank, except for traits."
instance HasModifiersFor ThrorsKey where
  getModifiersFor (ThrorsKey a) = case a.placement of
    AttachedTo location@(LocationTarget _) -> modified a location [Blank]
    _ -> pure ()

-- | Response: After a location is added to the staging area, attach Thrór's Key to that
-- location. One immune to player card effects cannot take it.
instance HasAbilities ThrorsKey where
  getAbilities (ThrorsKey a) =
    [ mkAbility a 1 $ response $ LocationEntersPlayWindow After
    | AttachedTo (HeroTarget _) <- [a.placement]
    ]

instance RunMessage ThrorsKey where
  runMessage msg s@(ThrorsKey a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (take 1 [lid | Window _ (EntersStagingArea (LocationTarget lid)) <- windows]) \lid ->
        whenM (selectAny $ LocationWithId lid <> LocationNotImmuneToPlayerCards) $ moveAttachment a.id (LocationTarget lid)
      pure s
    _ -> ThrorsKey <$> runMessage msg a
