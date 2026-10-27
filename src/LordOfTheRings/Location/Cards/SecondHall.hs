module LordOfTheRings.Location.Cards.SecondHall (secondHall) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype SecondHall = SecondHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

secondHall :: LocationCard SecondHall
secondHall = location SecondHall Cards.secondHall

-- | While Second Hall is the active location, it gains: "Forced: After a character leaves
-- play, reveal 1 card from the encounter deck and add it to the staging area."
instance HasAbilities SecondHall where
  getAbilities (SecondHall a) =
    [mkAbility a 1 $ forced $ CharacterLeftPlayWindow After AnyCard | a.placement == ActiveLocation]

instance RunMessage SecondHall where
  runMessage msg l@(SecondHall attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealEncounterCard
      pure l
    _ -> SecondHall <$> runMessage msg attrs
