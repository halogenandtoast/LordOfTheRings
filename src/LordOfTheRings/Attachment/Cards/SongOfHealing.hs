module LordOfTheRings.Attachment.Cards.SongOfHealing (songOfHealing) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype SongOfHealing = SongOfHealing AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero. Limit 1 per hero, on its card def.
songOfHealing :: AttachmentCard SongOfHealing
songOfHealing = attachment SongOfHealing Cards.songOfHealing & attachTo (AttachToHero AnyHero)

-- | Action: Discard 1 card from your hand to heal 1 damage from attached hero. Any player
-- may trigger this effect.
instance HasAbilities SongOfHealing where
  getAbilities (SongOfHealing a) =
    [ anyPlayer $ restricted a 1 (PlayerExists $ ActivePlayer <> PlayerWithCardsInHand 1) (ActionAbility Free)
    | AttachedTo (HeroTarget _) <- [a.placement]
    ]

instance RunMessage SongOfHealing where
  runMessage msg s@(SongOfHealing a) = case msg of
    -- the player who triggers it discards from their own hand, as Éowyn's
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      chooseAndDiscardFromHand pid a 1
      withHost a \host -> healDamage a host 1
      pure s
    _ -> SongOfHealing <$> runMessage msg a
