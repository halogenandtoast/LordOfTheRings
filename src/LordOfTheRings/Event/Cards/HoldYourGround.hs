module LordOfTheRings.Event.Cards.HoldYourGround (holdYourGround) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Sentinel))

newtype HoldYourGround = HoldYourGround EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose and ready a sentinel character.
-- Valour Action: Ready all sentinel characters.
holdYourGround :: EventCard HoldYourGround
holdYourGround = event HoldYourGround Cards.holdYourGround

instance RunMessage HoldYourGround where
  runMessage msg e@(HoldYourGround attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      sentinels <- selectCharacters (HeroWithKeyword Sentinel) (AllyWithKeyword Sentinel)
      chooseOrRunOneM pid do
        questionLabeled "holdYourGround.chooseOne"
        causedBy attrs
        targets sentinels ready
        valourLabeled pid $ for_ sentinels ready
      pure e
    _ -> HoldYourGround <$> runMessage msg attrs
