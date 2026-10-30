module LordOfTheRings.Attachment.Cards.HobbitPony (hobbitPony) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Trait (Trait (Hobbit))

newtype HobbitPony = HobbitPony AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Hobbit hero. Restricted, on its card def.
hobbitPony :: AttachmentCard HobbitPony
hobbitPony = attachment HobbitPony Cards.hobbitPony & attachTo (AttachToHero $ HeroWithTrait Hobbit)

-- | Quest Action: If attached hero is not committed to the quest, exhaust Hobbit Pony and
-- attached hero to commit attached hero to the quest.
instance HasAbilities HobbitPony where
  getAbilities (HobbitPony a) =
    [ restricted a 1 (DuringPhase QuestPhase <> exists (HeroWithId hid <> not_ CommittedHero))
        $ ActionAbility (Costs [exhaustCost a, ExhaustCost host])
    | AttachedTo host@(HeroTarget hid) <- [a.placement]
    ]

instance RunMessage HobbitPony where
  runMessage msg s@(HobbitPony a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a commitToQuest
      pure s
    _ -> HobbitPony <$> runMessage msg a
