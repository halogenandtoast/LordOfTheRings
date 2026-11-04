module LordOfTheRings.Attachment.Cards.FellowshipOfTheRing (fellowshipOfTheRing) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (RingBearer))

newtype FellowshipOfTheRing = FellowshipOfTheRing AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to the Ring-bearer.
fellowshipOfTheRing :: AttachmentCard FellowshipOfTheRing
fellowshipOfTheRing = attachment FellowshipOfTheRing Cards.fellowshipOfTheRing & attachTo (AttachToHero $ HeroWithTrait RingBearer)

-- | Each hero gets +1 [willpower].
instance HasModifiersFor FellowshipOfTheRing where
  getModifiersFor (FellowshipOfTheRing a) = withHost a \_ -> do
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [WillpowerModifier 1]

-- | Forced: After a character is destroyed, discard Fellowship of the Ring.
instance HasAbilities FellowshipOfTheRing where
  getAbilities (FellowshipOfTheRing a) = [mkAbility a 1 $ forced $ CharacterDestroyedWindow After]

instance RunMessage FellowshipOfTheRing where
  runMessage msg s@(FellowshipOfTheRing a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discard a
      pure s
    _ -> FellowshipOfTheRing <$> runMessage msg a
