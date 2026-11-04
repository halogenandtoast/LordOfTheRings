module LordOfTheRings.Event.Cards.WindFromTheSea (windFromTheSea) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype WindFromTheSea = WindFromTheSea EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Encounter. Never played from hand: set aside, it comes into the encounter deck only as
-- Flight to the Sea shuffles it in.
windFromTheSea :: EventCard WindFromTheSea
windFromTheSea = event WindFromTheSea Cards.windFromTheSea

{- | When Revealed: Ready each hero committed to the quest and remove Wind from the Sea from
the game. The game removes it from the game as it resolves.
Shadow: Ready the defending character.
-}
instance RunMessage WindFromTheSea where
  runMessage msg e@(WindFromTheSea attrs) = case msg of
    WhenRevealed (CardIdTarget cid) | cid == toId attrs -> do
      traverse_ ready =<< select CommittedHero
      pure e
    ResolveShadowEffect cid _ _ defenders | cid == toId attrs -> do
      traverse_ ready defenders
      pure e
    _ -> WindFromTheSea <$> runMessage msg attrs
