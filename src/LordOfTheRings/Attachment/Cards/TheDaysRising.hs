module LordOfTheRings.Attachment.Cards.TheDaysRising (theDaysRising) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Sentinel))

newtype TheDaysRising = TheDaysRising AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero with sentinel.
theDaysRising :: AttachmentCard TheDaysRising
theDaysRising = attachment TheDaysRising Cards.theDaysRising & attachTo (AttachToHero $ HeroWithKeyword Sentinel)

-- | Response: After attached hero defends against an attack and takes no damage while
-- defending that attack, exhaust The Day's Rising to add 1 resource to the attached hero's
-- resource pool.
instance HasAbilities TheDaysRising where
  getAbilities (TheDaysRising a) =
    [ mkAbility a 1 $ ResponseAbility (DefendedWithoutDamageWindow After host) (exhaustCost a)
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage TheDaysRising where
  runMessage msg s@(TheDaysRising a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> placeResources a host 1
      pure s
    _ -> TheDaysRising <$> runMessage msg a
