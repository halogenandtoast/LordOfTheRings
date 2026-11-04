module LordOfTheRings.Location.Cards.TheArgonath (theArgonath) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype TheArgonath = TheArgonath LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theArgonath :: LocationCard TheArgonath
theArgonath = location TheArgonath Cards.theArgonath

{- | Immune to player card effects. X is the number of players in the game: its threat.
The players cannot travel here while Sarn Gebir is in play. While The Argonath is the
active location, skip the combat phase.
-}
instance HasModifiersFor TheArgonath where
  getModifiersFor (TheArgonath a) = do
    x <- length <$> getRemainingPlayers
    gebir <- selectAny (LocationWithCardCode $ toCardCode Cards.sarnGebir)
    modifySelf a $ [ImmuneToPlayerCardEffects, ThreatModifier x] <> [CannotBeTraveledTo | gebir]
    when (a.placement == ActiveLocation) $ modified a GameTarget [SkipPhase CombatPhase]

instance RunMessage TheArgonath where
  runMessage msg (TheArgonath a) = TheArgonath <$> runMessage msg a
