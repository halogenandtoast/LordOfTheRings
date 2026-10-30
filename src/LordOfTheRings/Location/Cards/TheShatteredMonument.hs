module LordOfTheRings.Location.Cards.TheShatteredMonument (theShatteredMonument) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Trait (Trait (Raider))

newtype TheShatteredMonument = TheShatteredMonument LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

theShatteredMonument :: LocationCard TheShatteredMonument
theShatteredMonument = location TheShatteredMonument Cards.theShatteredMonument

-- | Each Raider enemy with at least 1 resource on it is immune to player events.
instance HasModifiersFor TheShatteredMonument where
  getModifiersFor (TheShatteredMonument a) = do
    raiders <- select (EnemyWithTrait Raider <> EnemyWithResources)
    for_ raiders \r -> modified a r [ImmuneToPlayerEvents]

-- | Forced: At the beginning of the resource phase, add 1 resource to each Raider enemy in
-- play.
instance HasAbilities TheShatteredMonument where
  getAbilities (TheShatteredMonument a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After ResourcePhase]

instance RunMessage TheShatteredMonument where
  runMessage msg l@(TheShatteredMonument a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      raiders <- select (EnemyWithTrait Raider)
      for_ raiders \r -> placeResources a r 1
      pure l
    _ -> TheShatteredMonument <$> runMessage msg a
