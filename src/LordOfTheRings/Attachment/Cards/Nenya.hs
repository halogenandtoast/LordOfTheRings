module LordOfTheRings.Attachment.Cards.Nenya (nenya) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Sphere (Sphere (Lore))

newtype Nenya = Nenya AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

nenya :: AttachmentCard Nenya
nenya = attachment Nenya Cards.nenya & attachTo (AttachToHero $ HeroWithTitle "Galadriel")

-- | Attach to Galadriel. She gains a lore resource icon.
instance HasModifiersFor Nenya where
  getModifiersFor (Nenya a) = withHost a \host -> modified a host [GainResourceIcon Lore]

-- | Quest Action: Exhaust Nenya and Galadriel to add her willpower to another
-- character's willpower until the end of the phase.
instance HasAbilities Nenya where
  getAbilities (Nenya a) =
    [ restricted a 1 (DuringPhase QuestPhase) $ ActionAbility (Costs [ExhaustCost (toTarget a), ExhaustCost host])
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage Nenya where
  runMessage msg s@(Nenya a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      withHost a \host -> do
        willpower <- getWillpower host
        others <- filter (/= host) <$> selectCharacters AnyHero AnyAlly
        chooseOneM pid do
          questionLabeled "nenya.give"
          causedBy a
          targets others \c -> phaseModifier a c (WillpowerModifier willpower)
      pure s
    _ -> Nenya <$> runMessage msg a
