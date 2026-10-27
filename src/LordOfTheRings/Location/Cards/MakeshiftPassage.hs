module LordOfTheRings.Location.Cards.MakeshiftPassage (makeshiftPassage) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype MakeshiftPassage = MakeshiftPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

makeshiftPassage :: LocationCard MakeshiftPassage
makeshiftPassage = location MakeshiftPassage Cards.makeshiftPassage

-- | Forced: After you travel to Makeshift Passage, place 2 progress tokens on the current
-- quest card, bypassing any active location.
instance HasAbilities MakeshiftPassage where
  getAbilities (MakeshiftPassage a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

instance RunMessage MakeshiftPassage where
  runMessage msg l@(MakeshiftPassage a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeProgressOnCurrentQuest a 2
      pure l
    _ -> MakeshiftPassage <$> runMessage msg a
