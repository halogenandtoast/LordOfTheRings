module LordOfTheRings.Event.Cards.SwiftAndSilent (swiftAndSilent) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype SwiftAndSilent = SwiftAndSilent EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Ready a hero you control. Then, if your threat is 20 or less and this is the
-- first time you played a copy of Swift and Silent this round, return this card to your
-- hand instead of discarding it.
swiftAndSilent :: EventCard SwiftAndSilent
swiftAndSilent = event SwiftAndSilent Cards.swiftAndSilent

instance RunMessage SwiftAndSilent where
  runMessage msg e@(SwiftAndSilent attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "chooseHeroToReady"
        causedBy attrs
        targets heroes ready
      returnIfFirstCopyThisRound pid card
      pure e
    _ -> SwiftAndSilent <$> runMessage msg attrs
