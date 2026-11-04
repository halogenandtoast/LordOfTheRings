module LordOfTheRings.Event.Cards.BurstIntoSong (burstIntoSong) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Song))

newtype BurstIntoSong = BurstIntoSong EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

burstIntoSong :: EventCard BurstIntoSong
burstIntoSong = eventWith BurstIntoSong Cards.burstIntoSong (criteriaL .~ exists (HeroWithAttached Song <> ExhaustedHero))

-- | Action: Ready each hero with a Song attachment.
instance RunMessage BurstIntoSong where
  runMessage msg e@(BurstIntoSong attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      traverse_ ready =<< select (HeroWithAttached Song)
      pure e
    _ -> BurstIntoSong <$> runMessage msg attrs
