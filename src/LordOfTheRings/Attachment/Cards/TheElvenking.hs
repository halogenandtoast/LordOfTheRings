module LordOfTheRings.Attachment.Cards.TheElvenking (theElvenking) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Silvan))

newtype TheElvenking = TheElvenking AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Silvan hero.
theElvenking :: AttachmentCard TheElvenking
theElvenking = attachment TheElvenking Cards.theElvenking & attachTo (AttachToHero $ HeroWithTrait Silvan)

-- | Action: Exhaust The Elvenking and return a Silvan ally you control to your hand to
-- ready attached hero.
instance HasAbilities TheElvenking where
  getAbilities (TheElvenking a) =
    [ restricted a 1 (exists $ AllyControlledBy (PlayerWithId a.controller) <> AllyWithTrait Silvan)
        $ ActionAbility (exhaustCost a)
    | AttachedTo _ <- [a.placement]
    ]

instance RunMessage TheElvenking where
  runMessage msg s@(TheElvenking a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      returnAllyToHand pid a (AllyWithTrait Silvan)
      withHost a ready
      pure s
    _ -> TheElvenking <$> runMessage msg a
