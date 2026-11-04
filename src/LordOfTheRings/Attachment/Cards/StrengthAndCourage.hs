module LordOfTheRings.Attachment.Cards.StrengthAndCourage (strengthAndCourage) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (cdAttack, toCardDef)

newtype StrengthAndCourage = StrengthAndCourage AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to the hero with The One Ring. Limit 1 per hero, on its card def.
strengthAndCourage :: AttachmentCard StrengthAndCourage
strengthAndCourage = attachment StrengthAndCourage Cards.strengthAndCourage & attachTo (AttachToHero heroWithTheOneRing)

-- | Attached hero gets +1 attack.
instance HasModifiersFor StrengthAndCourage where
  getModifiersFor (StrengthAndCourage a) = withHost a \host -> modified a host [AttackModifier 1]

-- | Response: After attached hero is declared as an attacker, exhaust The One Ring and raise
-- your threat by 1 to double attached hero's printed attack for this attack.
instance HasAbilities StrengthAndCourage where
  getAbilities (StrengthAndCourage a) =
    [ mkAbility a 1 $ ResponseAbility (AttackerDeclaredWindow After (Just host)) theOneRingCost
    | AttachedTo host@(HeroTarget _) <- [a.placement]
    ]

-- | Doubling the printed attack adds it once more, whatever else modifies the hero.
instance RunMessage StrengthAndCourage where
  runMessage msg s@(StrengthAndCourage a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \case
        host@(HeroTarget hid) -> do
          printed <- fromMaybe 0 . cdAttack . toCardDef <$> getHero hid
          attackModifier a host (AttackModifier printed)
        _ -> pure ()
      pure s
    _ -> StrengthAndCourage <$> runMessage msg a
