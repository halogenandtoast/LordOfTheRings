module LordOfTheRings.Location.Cards.OldForestRoad (oldForestRoad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype OldForestRoad = OldForestRoad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

oldForestRoad :: LocationCard OldForestRoad
oldForestRoad = location OldForestRoad Cards.oldForestRoad

-- | Response: After you travel to Old Forest Road, the first player may choose and ready 1
-- character he controls.
instance HasAbilities OldForestRoad where
  getAbilities (OldForestRoad a) = [mkAbility a 1 $ response $ TraveledWindow After a.id]

instance RunMessage OldForestRoad where
  runMessage msg l@(OldForestRoad a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      exhausted <- selectCharacters (HeroControlledBy (PlayerWithId lead) <> ExhaustedHero) (AllyControlledBy (PlayerWithId lead) <> ExhaustedAlly)
      chooseOneM lead do
        questionLabeled "oldForestRoad.ready"
        causedBy a
        targets exhausted ready
      pure l
    _ -> OldForestRoad <$> runMessage msg a
