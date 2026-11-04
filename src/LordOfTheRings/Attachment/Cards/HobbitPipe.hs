module LordOfTheRings.Attachment.Cards.HobbitPipe (hobbitPipe) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype HobbitPipe = HobbitPipe AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Hobbit character. Limit 1 per character, on its card def.
hobbitPipe :: AttachmentCard HobbitPipe
hobbitPipe = attachment HobbitPipe Cards.hobbitPipe & attachTo (AttachToCharacterMatching (HeroWithTrait Hobbit) (AllyWithTrait Hobbit))

-- | Response: After your threat is reduced by an event card effect, exhaust Hobbit Pipe to
-- draw a card.
instance HasAbilities HobbitPipe where
  getAbilities (HobbitPipe a) = [mkAbility a 1 $ ResponseAbility (ThreatReducedByEventWindow After a.controller) (exhaustCost a)]

instance RunMessage HobbitPipe where
  runMessage msg s@(HobbitPipe a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> HobbitPipe <$> runMessage msg a
