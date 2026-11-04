module LordOfTheRings.Event.Cards.DefiantChallenge (defiantChallenge) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype DefiantChallenge = DefiantChallenge EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Doomed 2, on its card def. Action: Each player may ready a character he controls.
defiantChallenge :: EventCard DefiantChallenge
defiantChallenge = event DefiantChallenge Cards.defiantChallenge

instance RunMessage DefiantChallenge where
  runMessage msg e@(DefiantChallenge attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      eachPlayer \p -> do
        heroes <- select (HeroControlledBy (PlayerWithId p) <> ExhaustedHero)
        allies <- select (AllyControlledBy (PlayerWithId p) <> ExhaustedAlly)
        chooseOneM p do
          questionLabeled "defiantChallenge.ready"
          causedBy attrs
          targets heroes ready
          targets allies ready
          labeled "defiantChallenge.none" $ pure ()
      pure e
    _ -> DefiantChallenge <$> runMessage msg attrs
