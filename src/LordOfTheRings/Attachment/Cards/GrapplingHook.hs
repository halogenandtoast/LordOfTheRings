module LordOfTheRings.Attachment.Cards.GrapplingHook (grapplingHook) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype GrapplingHook = GrapplingHook AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a character.
grapplingHook :: AttachmentCard GrapplingHook
grapplingHook = attachment GrapplingHook Cards.grapplingHook & attachTo AttachToCharacter

-- | Quest Action: Discard Grappling Hook and exhaust attached character to commit attached
-- character to the quest, using its attack instead of its willpower (or instead of its
-- defense if the current quest has the siege keyword).
instance HasAbilities GrapplingHook where
  getAbilities (GrapplingHook a) =
    [ restricted a 1 (DuringPhase QuestPhase <> uncommitted) $ ActionAbility (ExhaustCost host)
    | AttachedTo host <- [a.placement]
    , uncommitted <- case host of
        HeroTarget hid -> [exists (HeroWithId hid <> not_ CommittedHero)]
        AllyTarget aid -> [exists (AllyWithId aid <> not_ CommittedAlly)]
        _ -> []
    ]

instance RunMessage GrapplingHook where
  runMessage msg s@(GrapplingHook a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discard a
      withHost a \host -> do
        commitToQuest host
        phaseModifier a host QuestsWithAttack
      pure s
    _ -> GrapplingHook <$> runMessage msg a
