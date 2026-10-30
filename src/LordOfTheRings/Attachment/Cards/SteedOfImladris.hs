module LordOfTheRings.Attachment.Cards.SteedOfImladris (steedOfImladris) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))
import LordOfTheRings.Trait (Trait (Noldor))

newtype SteedOfImladris = SteedOfImladris AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a spirit or Noldor hero. Restricted, on its card def.
steedOfImladris :: AttachmentCard SteedOfImladris
steedOfImladris =
  attachment SteedOfImladris Cards.steedOfImladris
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithSphere Spirit, HeroWithTrait Noldor])

-- | Response: After attached hero commits to a quest, discard a card from your hand to
-- place 2 progress on the active location.
instance HasAbilities SteedOfImladris where
  getAbilities (SteedOfImladris a) =
    [ restricted a 1 (exists IsActiveLocation)
        $ ResponseAbility (CommittedToQuestWindow After host) (DiscardFromHandCost a.controller 1)
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage SteedOfImladris where
  runMessage msg s@(SteedOfImladris a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      active <- select IsActiveLocation
      for_ (take 1 active) \lid -> placeProgress a lid 2
      pure s
    _ -> SteedOfImladris <$> runMessage msg a
