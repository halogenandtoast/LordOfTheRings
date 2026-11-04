module LordOfTheRings.Location.Cards.CrumblingPassage (crumblingPassage) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype CrumblingPassage = CrumblingPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

crumblingPassage :: LocationCard CrumblingPassage
crumblingPassage = location CrumblingPassage Cards.crumblingPassage

-- | Forced: When Crumbling Passage leaves play, deal 1 damage to each exhausted character.
-- Read in the window as it leaves, which nothing comes before.
instance HasAbilities CrumblingPassage where
  getAbilities (CrumblingPassage a) = [mkAbility a 1 $ forced $ LocationLeftPlayWindow After a.id]

-- | When Revealed: Make Crumbling Passage the active location, returning any previously
-- active location to the staging area.
instance RunMessage CrumblingPassage where
  runMessage msg l@(CrumblingPassage a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      active <- select IsActiveLocation
      for_ active \x -> placeLocation x StagingArea
      makeActive a.id
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      for_ exhausted \c -> dealDamage a c 1
      pure l
    _ -> CrumblingPassage <$> runMessage msg a
