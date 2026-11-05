module LordOfTheRings.Location.Cards.DarkPassages (darkPassages) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Cave))

newtype DarkPassages = DarkPassages LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkPassages :: LocationCard DarkPassages
darkPassages = location DarkPassages Cards.darkPassages

instance RunMessage DarkPassages where
  runMessage msg l@(DarkPassages attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      locations <-
        filterM (fmap (not . cardMatches (CardWithTitle "Dark Passages")) . getLocation)
          =<< select LocationInStagingArea
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "darkPassages.makeActive"
        causedBy attrs
        targets locations makeActive
      pure l
    TravelTo lid | lid == attrs.id -> do
      caves <- length <$> select (LocationInStagingArea <> LocationWithTrait Cave)
      eachPlayer \p -> raiseThreat p caves
      DarkPassages <$> runMessage msg attrs
    _ -> DarkPassages <$> runMessage msg attrs
