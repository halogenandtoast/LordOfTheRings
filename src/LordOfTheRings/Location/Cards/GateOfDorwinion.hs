module LordOfTheRings.Location.Cards.GateOfDorwinion (gateOfDorwinion) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests

newtype GateOfDorwinion = GateOfDorwinion LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

gateOfDorwinion :: LocationCard GateOfDorwinion
gateOfDorwinion = location GateOfDorwinion Cards.gateOfDorwinion

-- | Immune to player card effects. X is 1 more than the number of locations controlled by
-- the players. The players cannot travel here unless stage 2B has at least 12 progress on
-- it.
instance HasModifiersFor GateOfDorwinion where
  getModifiersFor (GateOfDorwinion a) = do
    x <- (+ 1) . length <$> select (LocationControlledBy AnyPlayer)
    reached <-
      getsGame (any (\q -> toCardCode q == toCardCode Quests.driveThemOut && q.flipped && q.progress >= 12) . toList . (.entities.quests))
    modifySelf a $ [ImmuneToPlayerCardEffects, ThreatModifier x] <> [CannotBeTraveledTo | not reached]

-- | Travel: Thane Ulchor attacks each player in turn order.
instance RunMessage GateOfDorwinion where
  runMessage msg l@(GateOfDorwinion a) = case msg of
    TravelTo lid | lid == a.id -> do
      thanes <- select thaneUlchor
      eachPlayer \pid -> for_ thanes (immediateAttack pid)
      pure l
    _ -> GateOfDorwinion <$> runMessage msg a
