module LordOfTheRings.Location.Cards.HallOfHorrors (hallOfHorrors) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTempleOfDoom.Helpers (templeX)

newtype HallOfHorrors = HallOfHorrors LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

hallOfHorrors :: LocationCard HallOfHorrors
hallOfHorrors = location HallOfHorrors Cards.hallOfHorrors

-- | X is 1 more than the number of quest stages in the victory display. Its threat is X.
instance HasModifiersFor HallOfHorrors where
  getModifiersFor (HallOfHorrors a) = do
    x <- templeX
    modifySelf a [ThreatModifier x]

-- | Forced: When Hall of Horrors leaves play, each player deals X damage to a character he
-- controls. Read in the window as it leaves, which nothing comes before.
instance HasAbilities HallOfHorrors where
  getAbilities (HallOfHorrors a) = [mkAbility a 1 $ forced $ LocationLeftPlayWindow After a.id]

instance RunMessage HallOfHorrors where
  runMessage msg l@(HallOfHorrors a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      x <- templeX
      eachPlayer \p -> dealDamageToOneCharacter a p x
      pure l
    _ -> HallOfHorrors <$> runMessage msg a
