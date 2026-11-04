module LordOfTheRings.Attachment.Cards.LeaderOfMen (leaderOfMen) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Noble))

newtype LeaderOfMen = LeaderOfMen AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Treason of Saruman. Permanent, on its card def. Setup: Attach to a Noble
-- hero: as the Campaign Pool records it on its hero.
leaderOfMen :: AttachmentCard LeaderOfMen
leaderOfMen = attachment LeaderOfMen Cards.leaderOfMen & attachTo (AttachToHero $ HeroWithTrait Noble)

-- | Response: After attached hero quests successfully, exhaust Leader of Men to add 1
-- resource to attached hero's pool: the players quested successfully with it committed.
instance HasAbilities LeaderOfMen where
  getAbilities (LeaderOfMen a) =
    [ restricted a 1 (exists $ HeroWithId hid <> CommittedHero) $ ResponseAbility (QuestedSuccessfullyWindow After) (exhaustCost a)
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

instance RunMessage LeaderOfMen where
  runMessage msg s@(LeaderOfMen a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> placeResources a host 1
      pure s
    _ -> LeaderOfMen <$> runMessage msg a
