module LordOfTheRings.Attachment.Cards.ElvenSpear (elvenSpear) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Noldor, Silvan))

newtype ElvenSpear = ElvenSpear AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Noldor or Silvan hero. Restricted, on its card def.
elvenSpear :: AttachmentCard ElvenSpear
elvenSpear =
  attachment ElvenSpear Cards.elvenSpear
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithTrait Noldor, HeroWithTrait Silvan])

-- | Action: Discard a card from your hand to give attached hero +1 attack until the end of
-- the phase. Limit 3 times per phase.
instance HasAbilities ElvenSpear where
  getAbilities (ElvenSpear a) =
    [ limited (PlayerLimit PerPhase 3) $ mkAbility a 1 $ ActionAbility (DiscardFromHandCost a.controller 1)
    | AttachedTo _ <- [a.placement]
    ]

instance RunMessage ElvenSpear where
  runMessage msg s@(ElvenSpear a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> phaseModifier a host (AttackModifier 1)
      pure s
    _ -> ElvenSpear <$> runMessage msg a
