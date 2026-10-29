module LordOfTheRings.Location.Cards.GrimFoothills (grimFoothills) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype GrimFoothills = GrimFoothills LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

grimFoothills :: LocationCard GrimFoothills
grimFoothills = location GrimFoothills Cards.grimFoothills

-- | X is the number of players in the game. While Grim Foothills is in the staging area,
-- progress must be placed on Grim Foothills before it can be placed on the active location.
instance HasModifiersFor GrimFoothills where
  getModifiersFor (GrimFoothills a) = do
    players <- length <$> getRemainingPlayers
    modified a a $ ThreatModifier players : [TakesProgressBeforeActiveLocation | a.placement == StagingArea]

instance RunMessage GrimFoothills where
  runMessage msg (GrimFoothills a) = GrimFoothills <$> runMessage msg a
