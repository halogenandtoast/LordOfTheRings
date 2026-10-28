module LordOfTheRings.Location.Cards.TheFourthStar (theFourthStar) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheFourthStar = TheFourthStar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFourthStar :: LocationCard TheFourthStar
theFourthStar = location TheFourthStar Cards.theFourthStar

-- | Underworld X. X is the number of players in the game.
-- Response: When The Fourth Star leaves play as an explored location, each player may
-- draw 1 card.
instance HasAbilities TheFourthStar where
  getAbilities (TheFourthStar a) = [mkAbility a 1 $ response $ ExploredWindow When a.id]

instance RunMessage TheFourthStar where
  runMessage msg l@(TheFourthStar a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> chooseOneM pid do
        questionLabeled "theFourthStar.draw"
        causedBy a
        labeled "theFourthStar.drawCard" $ drawCards pid 1
        labeled "pass" $ pure ()
      pure l
    _ -> TheFourthStar <$> runMessage msg a
