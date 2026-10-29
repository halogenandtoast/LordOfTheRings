module LordOfTheRings.Location.Cards.TangledWoods (tangledWoods) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Forest))

newtype TangledWoods = TangledWoods LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

tangledWoods :: LocationCard TangledWoods
tangledWoods = location TangledWoods Cards.tangledWoods

-- | Only the first copy in the staging area gives its ability: it does not stack.
givesTravelCost :: HasGame m => LocationAttrs -> m Bool
givesTravelCost a
  | a.placement /= StagingArea = pure False
  | otherwise = (== Just a.id) . minimumMay <$> select (LocationWithCardCode (toCardCode Cards.tangledWoods) <> LocationInStagingArea)

-- | While Tangled Woods is in the staging area, each Forest location in play gains: "Travel:
-- Exhaust a hero to travel here." This ability does not stack with other copies of Tangled
-- Woods.
instance HasModifiersFor TangledWoods where
  getModifiersFor (TangledWoods a) = do
    gives <- givesTravelCost a
    stuck <- selectNone ReadyHero
    when (gives && stuck) do
      forests <- select (LocationWithTrait Forest)
      for_ forests \l -> modified a l [CannotBeTraveledTo]

instance RunMessage TangledWoods where
  runMessage msg (TangledWoods a) = case msg of
    TravelTo lid -> do
      gives <- givesTravelCost a
      forest <- selectAny (LocationWithId lid <> LocationWithTrait Forest)
      when (gives && forest) do
        lead <- getLead
        heroes <- select ReadyHero
        chooseOneM lead do
          questionLabeled "tangledWoods.exhaustHero"
          causedBy a
          targets heroes exhaust
      TangledWoods <$> runMessage msg a
    _ -> TangledWoods <$> runMessage msg a
