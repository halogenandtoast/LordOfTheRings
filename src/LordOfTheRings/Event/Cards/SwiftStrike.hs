module LordOfTheRings.Event.Cards.SwiftStrike (swiftStrike) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype SwiftStrike = SwiftStrike EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After a character is declared as a defender, deal 2 damage to the
-- attacking enemy.
swiftStrike :: EventCard SwiftStrike
swiftStrike = eventWith SwiftStrike Cards.swiftStrike (windowL ?~ DefenderDeclaredWindow After Nothing)

instance RunMessage SwiftStrike where
  runMessage msg e@(SwiftStrike attrs) = case msg of
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ (headMay [eid | Window _ (DeclaredAsDefender _ eid) <- windows]) \eid -> dealDamage attrs eid 2
      pure e
    _ -> SwiftStrike <$> runMessage msg attrs
