module LordOfTheRings.Location.Cards.EchoingPassage (echoingPassage) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype EchoingPassage = EchoingPassage LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

echoingPassage :: LocationCard EchoingPassage
echoingPassage = location EchoingPassage Cards.echoingPassage

-- | Echoing Passage gets +1 threat for each enemy in the staging area. While Echoing
-- Passage is the active location, each enemy gets +2 defense.
instance HasModifiersFor EchoingPassage where
  getModifiersFor (EchoingPassage a) = do
    n <- length <$> select EnemyInStagingArea
    when (n > 0) $ modifySelf a [ThreatModifier n]
    when (a.placement == ActiveLocation) do
      enemies <- select AnyEnemy
      for_ enemies \e -> modified a e [DefenseModifier 2]

instance RunMessage EchoingPassage where
  runMessage msg (EchoingPassage a) = EchoingPassage <$> runMessage msg a
