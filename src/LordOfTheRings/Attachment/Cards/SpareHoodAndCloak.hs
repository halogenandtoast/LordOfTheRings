module LordOfTheRings.Attachment.Cards.SpareHoodAndCloak (spareHoodAndCloak) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource)

newtype SpareHoodAndCloak = SpareHoodAndCloak AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a character.
spareHoodAndCloak :: AttachmentCard SpareHoodAndCloak
spareHoodAndCloak = attachment SpareHoodAndCloak Cards.spareHoodAndCloak & attachTo AttachToCharacter

-- | Action: Exhaust Spare Hood and Cloak and exhaust attached character to ready another
-- character. Then, attach Spare Hood and Cloak to that character.
instance HasAbilities SpareHoodAndCloak where
  getAbilities (SpareHoodAndCloak a) =
    [ restricted a 1 (uncurry (CharactersAtLeast 1) (otherThan host)) $ ActionAbility (Costs [exhaustCost a, ExhaustCost host])
    | AttachedTo host <- [a.placement]
    ]

-- | The characters other than the host.
otherThan :: Target -> (HeroMatcher, AllyMatcher)
otherThan = \case
  HeroTarget hid -> (NotHero (HeroWithId hid), AnyAlly)
  AllyTarget aid -> (AnyHero, NotAlly (AllyWithId aid))
  _ -> (AnyHero, AnyAlly)

-- | The character readied is one the cloak can go on, as one immune to player card effects
-- or with no attachments allowed, as Beorn, is not.
instance RunMessage SpareHoodAndCloak where
  runMessage msg s@(SpareHoodAndCloak a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      for_ [host | AttachedTo host <- [a.placement]] \host -> do
        others <- uncurry selectCharacters (otherThan host)
        characters <- flip filterM others \c ->
          andM [not <$> immuneToSource (toSource a) c, notElem CannotHaveAttachments <$> getModifiers c]
        chooseOrRunOneM pid do
          questionLabeled "spareHoodAndCloak.chooseCharacter"
          causedBy a
          targets characters \c -> do
            ready c
            moveAttachment a.id c
      pure s
    _ -> SpareHoodAndCloak <$> runMessage msg a
