module LordOfTheRings.Attachment.Cards.SteedOfTheNorth (steedOfTheNorth) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Dunedain, Ranger))

newtype SteedOfTheNorth = SteedOfTheNorth AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dúnedain or Ranger hero. Restricted, on its card def.
steedOfTheNorth :: AttachmentCard SteedOfTheNorth
steedOfTheNorth =
  attachment SteedOfTheNorth Cards.steedOfTheNorth
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithTrait Dunedain, HeroWithTrait Ranger])

-- | Response: After you engage an enemy, exhaust Steed of the North to ready attached hero.
instance HasAbilities SteedOfTheNorth where
  getAbilities (SteedOfTheNorth a) =
    [ restricted a 1 (exists $ HeroWithId hid <> ExhaustedHero)
        $ ResponseAbility (PlayerEngagedEnemyWindow After a.controller) (exhaustCost a)
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

instance RunMessage SteedOfTheNorth where
  runMessage msg s@(SteedOfTheNorth a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a ready
      pure s
    _ -> SteedOfTheNorth <$> runMessage msg a
