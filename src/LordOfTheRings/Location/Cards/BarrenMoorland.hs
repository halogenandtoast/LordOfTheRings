module LordOfTheRings.Location.Cards.BarrenMoorland (barrenMoorland) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype BarrenMoorland = BarrenMoorland LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

barrenMoorland :: LocationCard BarrenMoorland
barrenMoorland = location BarrenMoorland Cards.barrenMoorland

-- | While Barren Moorland is in the staging area, it gains: "Forced: When an ally enters
-- play, you must either exhaust it or deal 1 damage to it."
instance HasAbilities BarrenMoorland where
  getAbilities (BarrenMoorland a) = [mkAbility a 1 $ forced $ AllyEnteredPlayWindow After Nothing Nothing | a.placement == StagingArea]

-- | Travel: Deal 1 damage to each exhausted character.
instance RunMessage BarrenMoorland where
  runMessage msg l@(BarrenMoorland a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [(ally, pid) | Window _ (AllyEnteredPlay ally pid _) <- ws] \(ally, pid) ->
        chooseOneM pid do
          questionLabeled "barrenMoorland.choose"
          causedBy a
          labeled "barrenMoorland.exhaust" $ exhaust ally
          labeled "barrenMoorland.damage" $ dealDamage a ally 1
      pure l
    TravelTo lid | lid == a.id -> do
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      for_ exhausted \c -> dealDamage a c 1
      BarrenMoorland <$> runMessage msg a
    _ -> BarrenMoorland <$> runMessage msg a
