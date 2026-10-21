module LordOfTheRings.Event.Cards.ForthEorlingas (forthEorlingas) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Trait (Trait (Rohan))

newtype ForthEorlingas = ForthEorlingas EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Combat Action: Each Rohan hero can be declared as an attacker against enemies in the
-- staging area this phase.
forthEorlingas :: EventCard ForthEorlingas
forthEorlingas = eventWith ForthEorlingas Cards.forthEorlingas (criteriaL .~ DuringPhase CombatPhase)

instance RunMessage ForthEorlingas where
  runMessage msg e@(ForthEorlingas attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      heroes <- select (HeroWithTrait Rohan)
      for_ heroes \h -> phaseModifier attrs h CanAttackStagingArea
      pure e
    _ -> ForthEorlingas <$> runMessage msg attrs
