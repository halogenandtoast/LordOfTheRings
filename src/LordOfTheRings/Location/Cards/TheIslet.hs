module LordOfTheRings.Location.Cards.TheIslet (theIslet) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Trait (Trait (Dunland))

newtype TheIslet = TheIslet LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Gríma is attached to it at setup, and is free of encounters once it leaves play.
theIslet :: LocationCard TheIslet
theIslet = location TheIslet Cards.theIslet

-- | Immune to player card effects.
-- While The Islet is the active location, each Dunland enemy in play gets +1 threat.
instance HasModifiersFor TheIslet where
  getModifiersFor (TheIslet a) = do
    modified a a [ImmuneToPlayerCardEffects]
    when (a.placement == ActiveLocation) do
      enemies <- select (EnemyWithTrait Dunland)
      for_ enemies \e -> modified a e [ThreatModifier 1]
