module LordOfTheRings.Location.Cards.TheCarrock (theCarrock) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Trait (Trait (Troll))

newtype TheCarrock = TheCarrock LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCarrock :: LocationCard TheCarrock
theCarrock = location TheCarrock Cards.theCarrock

-- | Immune to player card effects. Players cannot travel to The Carrock except through
-- quest card effects. While The Carrock is the active location, Troll enemies get +1
-- attack and +1 defense.
instance HasModifiersFor TheCarrock where
  getModifiersFor (TheCarrock a) = do
    modified a a [ImmuneToPlayerCardEffects, CannotBeTraveledTo]
    when (a.placement == ActiveLocation) do
      trolls <- select (EnemyWithTrait Troll)
      for_ trolls \t -> modified a t [AttackModifier 1, DefenseModifier 1]
