module LordOfTheRings.Location.Cards.OvergrownPassage (overgrownPassage) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

-- | A Grotto deck card, over Jagged Cavern.
newtype OvergrownPassage = OvergrownPassage LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

overgrownPassage :: LocationCard OvergrownPassage
overgrownPassage = location OvergrownPassage Cards.overgrownPassage

-- | While Overgrown Passage is the active location, heroes cannot gain resources.
instance HasModifiersFor OvergrownPassage where
  getModifiersFor (OvergrownPassage a) = when (a.placement == ActiveLocation) do
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [CannotCollectResources, CannotGainResourcesFromCardEffects]

-- | Response: After you travel to Overgrown Passage, you may flip it to its Underwater side.
-- The first player decides.
instance HasAbilities OvergrownPassage where
  getAbilities (OvergrownPassage a) = [mkAbility a 1 $ silent $ TraveledWindow After a.id]

instance RunMessage OvergrownPassage where
  runMessage msg l@(OvergrownPassage a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "overgrownPassage.mayFlip"
        causedBy a
        labeled "overgrownPassage.flip" $ flipOver a
        labeled "overgrownPassage.stay" $ pure ()
      pure l
    _ -> OvergrownPassage <$> runMessage msg a
