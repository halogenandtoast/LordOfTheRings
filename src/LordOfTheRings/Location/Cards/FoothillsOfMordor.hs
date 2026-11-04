module LordOfTheRings.Location.Cards.FoothillsOfMordor (foothillsOfMordor) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype FoothillsOfMordor = FoothillsOfMordor LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

foothillsOfMordor :: LocationCard FoothillsOfMordor
foothillsOfMordor = location FoothillsOfMordor Cards.foothillsOfMordor

-- | While Foothills of Mordor is in the staging area, 'when revealed' effects cannot be
-- canceled.
instance HasModifiersFor FoothillsOfMordor where
  getModifiersFor (FoothillsOfMordor a) = when (a.placement == StagingArea) $ modified a GameTarget [WhenRevealedCannotBeCanceled]

instance HasAbilities FoothillsOfMordor where
  getAbilities (FoothillsOfMordor a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

-- | Forced: After Foothills of Mordor becomes the active location, raise each player's
-- threat by 2.
instance RunMessage FoothillsOfMordor where
  runMessage msg l@(FoothillsOfMordor a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> encounterRaisesThreat p 2
      pure l
    _ -> FoothillsOfMordor <$> runMessage msg a
