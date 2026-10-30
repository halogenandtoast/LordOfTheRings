module LordOfTheRings.Location.Cards.CursedCaverns (cursedCaverns) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

-- | Grotto side; its Underwater side is Twisting Hollow.
newtype CursedCaverns = CursedCaverns LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cursedCaverns :: LocationCard CursedCaverns
cursedCaverns = location CursedCaverns Cards.cursedCaverns

-- | Response: After you travel to Cursed Caverns, you may flip it to its Underwater side.
-- Response: After the players explore Cursed Caverns as the active location, each player
-- may raise his threat by 2 to draw the bottom card of his deck.
instance HasAbilities CursedCaverns where
  getAbilities (CursedCaverns a) =
    mkAbility a 1 (response $ TraveledToWindow After (LocationWithId a.id))
      : [mkAbility a 2 $ response $ ExploredWindow When a.id | a.placement == ActiveLocation]

instance RunMessage CursedCaverns where
  runMessage msg l@(CursedCaverns a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      flipOver a
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      eachPlayer \p -> do
        bottom <- lastMay . (.deck) <$> getPlayer p
        for_ bottom \card -> chooseOneM p do
          questionLabeled "cursedCaverns.draw"
          causedBy a
          labeled "cursedCaverns.raise" do
            raiseThreat p 2
            takeFromDeck p card
          labeled "cursedCaverns.decline" $ pure ()
      pure l
    _ -> CursedCaverns <$> runMessage msg a
