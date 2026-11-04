module LordOfTheRings.Location.Cards.HighFalls (highFalls) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype HighFalls = HighFalls LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Deep.
highFalls :: LocationCard HighFalls
highFalls = location HighFalls Cards.highFalls

-- | X is the number of locations in the staging area, High Falls among them while it is
-- there.
xOf :: HasGame m => m Int
xOf = length <$> select LocationInStagingArea

-- | X threat.
instance HasModifiersFor HighFalls where
  getModifiersFor (HighFalls a) = do
    x <- xOf
    when (x > 0) $ modifySelf a [ThreatModifier x]

-- | Travel: Assign X damage among characters in play to travel here. X is counted as the
-- players travel, High Falls still in the staging area; the first player assigns it.
instance RunMessage HighFalls where
  runMessage msg (HighFalls a) = case msg of
    TravelTo lid | lid == a.id -> do
      x <- xOf
      lead <- getLead
      characters <- selectCharacters AnyHero AnyAlly
      when (x > 0) $ dealDamageDividedAmong a lead characters x
      HighFalls <$> runMessage msg a
    _ -> HighFalls <$> runMessage msg a
