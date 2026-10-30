module LordOfTheRings.Attachment.Cards.Narya (narya) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership))

newtype Narya = Narya AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to Círdan the Shipwright or Gandalf.
narya :: AttachmentCard Narya
narya =
  attachment Narya Cards.narya
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithTitle "Círdan the Shipwright", HeroWithTitle "Gandalf"])
          (AllyWithTitle "Gandalf")
      )

-- | Attached character gains a leadership resource icon.
instance HasModifiersFor Narya where
  getModifiersFor (Narya a) = withHost a \host -> modified a host [GainResourceIcon Leadership]

-- | Action: Exhaust Narya and attached character to choose and ready up to 2 allies. Each of
-- those allies gets +1 attack and +1 defense until the end of the phase.
instance HasAbilities Narya where
  getAbilities (Narya a) =
    [mkAbility a 1 $ ActionAbility (Costs [exhaustCost a, ExhaustCost host]) | AttachedTo host <- [a.placement]]

instance RunMessage Narya where
  runMessage msg s@(Narya a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      allies <- select AnyAlly
      chooseOneM pid do
        questionLabeled "narya.ally"
        causedBy a
        targets allies \one -> do
          rally a one
          chooseOneM pid do
            questionLabeled "narya.ally"
            causedBy a
            targets (filter (/= one) allies) (rally a)
            labeled "narya.done" (pure ())
        labeled "narya.done" (pure ())
      pure s
    _ -> Narya <$> runMessage msg a

-- | The ally readies and gets +1 attack and +1 defense until the end of the phase.
rally :: ReverseQueue m => AttachmentAttrs -> AllyId -> m ()
rally a ally' = do
  ready ally'
  phaseModifiers a ally' [AttackModifier 1, DefenseModifier 1]
