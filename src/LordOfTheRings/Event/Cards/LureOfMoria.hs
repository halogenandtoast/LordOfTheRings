module LordOfTheRings.Event.Cards.LureOfMoria (lureOfMoria) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf))

newtype LureOfMoria = LureOfMoria EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Ready all Dwarf characters.
lureOfMoria :: EventCard LureOfMoria
lureOfMoria = event LureOfMoria Cards.lureOfMoria

instance RunMessage LureOfMoria where
  runMessage msg e@(LureOfMoria attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      traverse_ ready =<< selectCharacters (HeroWithTrait Dwarf <> ExhaustedHero) (AllyWithTrait Dwarf <> ExhaustedAlly)
      pure e
    _ -> LureOfMoria <$> runMessage msg attrs
