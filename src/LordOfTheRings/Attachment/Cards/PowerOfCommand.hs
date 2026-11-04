module LordOfTheRings.Attachment.Cards.PowerOfCommand (powerOfCommand) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype PowerOfCommand = PowerOfCommand AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to the hero with The One Ring. Limit 1 per hero, on its card def.
powerOfCommand :: AttachmentCard PowerOfCommand
powerOfCommand = attachment PowerOfCommand Cards.powerOfCommand & attachTo (AttachToHero heroWithTheOneRing)

-- | Attached hero gets +1 willpower.
instance HasModifiersFor PowerOfCommand where
  getModifiersFor (PowerOfCommand a) = withHost a \host -> modified a host [WillpowerModifier 1]

-- | Response: After attached hero commits to the quest, exhaust The One Ring and raise your
-- threat by 1 to give each other unique character you control +1 willpower until the end of
-- the phase.
instance HasAbilities PowerOfCommand where
  getAbilities (PowerOfCommand a) =
    [ mkAbility a 1 $ ResponseAbility (CommittedToQuestWindow After host) theOneRingCost
    | AttachedTo host@(HeroTarget _) <- [a.placement]
    ]

-- | Every hero is unique.
instance RunMessage PowerOfCommand where
  runMessage msg s@(PowerOfCommand a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \case
        HeroTarget hid -> do
          let you = PlayerWithId a.controller
          heroes <- select (HeroControlledBy you <> not_ (HeroWithId hid))
          allies <- select (AllyControlledBy you <> UniqueAlly)
          for_ heroes \h -> phaseModifier a h (WillpowerModifier 1)
          for_ allies \x -> phaseModifier a x (WillpowerModifier 1)
        _ -> pure ()
      pure s
    _ -> PowerOfCommand <$> runMessage msg a
