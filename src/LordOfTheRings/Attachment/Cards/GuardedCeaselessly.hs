module LordOfTheRings.Attachment.Cards.GuardedCeaselessly (guardedCeaselessly) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Ranger, Scout))

newtype GuardedCeaselessly = GuardedCeaselessly AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a location.
guardedCeaselessly :: AttachmentCard GuardedCeaselessly
guardedCeaselessly = attachment GuardedCeaselessly Cards.guardedCeaselessly & attachTo (AttachToLocation AnyLocation)

-- | Action: Exhaust a Ranger or Scout character to give attached location -2 threat until
-- the end of the phase.
instance HasAbilities GuardedCeaselessly where
  getAbilities (GuardedCeaselessly a) =
    [ mkAbility a 1 $ ActionAbility $ ExhaustCharacterCost watcherHero watcherAlly
    | AttachedTo _ <- [a.placement]
    ]
   where
    watcherHero = HeroControlledBy ActivePlayer <> HeroMatchAny [HeroWithTrait Ranger, HeroWithTrait Scout]
    watcherAlly = AllyControlledBy ActivePlayer <> AllyMatchAny [AllyWithTrait Ranger, AllyWithTrait Scout]

instance RunMessage GuardedCeaselessly where
  runMessage msg s@(GuardedCeaselessly a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> phaseModifier a host (ThreatModifier (-2))
      pure s
    _ -> GuardedCeaselessly <$> runMessage msg a
