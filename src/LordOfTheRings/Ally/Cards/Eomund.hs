module LordOfTheRings.Ally.Cards.Eomund (eomund) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Trait (Trait (Rohan))

newtype Eomund = Eomund AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eomund :: AllyCard Eomund
eomund = ally Eomund Cards.eomund

-- | Response: After Éomund leaves play, ready all Rohan characters in play.
instance RunMessage Eomund where
  runMessage msg (Eomund a) = case msg of
    -- ponytail: a card that has left play offers no abilities, so Éomund asks as he
    -- leaves; a general "just left play" ability would replace this
    _ | leaving, Just pid <- placementController a.placement -> do
      rohan <- filter (/= toTarget a) <$> selectCharacters (HeroWithTrait Rohan) (AllyWithTrait Rohan)
      chooseOneM pid do
        questionLabeled "eomund.response"
        causedBy a
        labeled "eomund.readyAll" $ for_ rohan ready
        labeled "pass" $ pure ()
      Eomund <$> runMessage msg a
    _ -> Eomund <$> runMessage msg a
   where
    leaving = case msg of
      Discard t -> t == toTarget a
      ReturnToHand t -> t == toTarget a
      ShuffleIntoOwnersDeck t -> t == toTarget a
      _ -> False
