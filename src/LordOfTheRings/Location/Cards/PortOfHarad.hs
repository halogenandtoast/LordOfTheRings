module LordOfTheRings.Location.Cards.PortOfHarad (portOfHarad) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype PortOfHarad = PortOfHarad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

portOfHarad :: LocationCard PortOfHarad
portOfHarad = location PortOfHarad Cards.portOfHarad

engagedEnemies :: HasGame m => m Int
engagedEnemies = length <$> select (EnemyEngagedWith AnyPlayer)

-- | X is the number of engaged enemies: its threat.
instance HasModifiersFor PortOfHarad where
  getModifiersFor (PortOfHarad a) = do
    x <- engagedEnemies
    modified a a [ThreatModifier x | x > 0]

-- | Travel: Assign X damage among characters in play to travel here. The first player
-- assigns it.
instance RunMessage PortOfHarad where
  runMessage msg (PortOfHarad a) = case msg of
    TravelTo lid | lid == a.id -> do
      x <- engagedEnemies
      lead <- getLead
      characters <- selectCharacters AnyHero AnyAlly
      when (x > 0) $ dealDamageDividedAmong a lead characters x
      PortOfHarad <$> runMessage msg a
    _ -> PortOfHarad <$> runMessage msg a
