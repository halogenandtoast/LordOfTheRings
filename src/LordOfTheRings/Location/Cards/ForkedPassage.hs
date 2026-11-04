module LordOfTheRings.Location.Cards.ForkedPassage (forkedPassage) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ForkedPassage = ForkedPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

forkedPassage :: LocationCard ForkedPassage
forkedPassage = location ForkedPassage Cards.forkedPassage

-- | Deep. Response: When you travel to Forked Passage (before resolving the deep keyword),
-- raise each player's threat by 2 to look at the top 2 cards of the Caves deck. Put 1 on
-- top of the Caves deck and put the other on the bottom. Then, add Forked Passage to the
-- victory display.
-- Offered in the window after traveling, which comes before Deep resolves.
instance HasAbilities ForkedPassage where
  getAbilities (ForkedPassage a) = [mkAbility a 1 $ response $ TraveledWindow After a.id]

{- | The deep keyword still resolves after the response: "The Deep keyword will resolve
whether you trigger this effect or not" (Caleb, FFG forum "Unofficial Official Rulings
thread", 4 May 2019). Forked Passage, in the victory display, is not there to discard, and
the top card of the Caves deck becomes the active location.
-}
instance RunMessage ForkedPassage where
  runMessage msg l@(ForkedPassage a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      eachPlayer (`raiseThreat` 2)
      arrangeTopOfCavesDeck pid a
      addToVictoryDisplay a
      ignored <- elem IgnoreDeep <$> getModifiers GameTarget
      unless ignored $ replaceWithTopOfCavesDeck a.id
      pure l
    _ -> ForkedPassage <$> runMessage msg a
