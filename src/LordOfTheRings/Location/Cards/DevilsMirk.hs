module LordOfTheRings.Location.Cards.DevilsMirk (devilsMirk) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation, StagingArea))
import LordOfTheRings.Trait (Trait (Wose))

newtype DevilsMirk = DevilsMirk LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

devilsMirk :: LocationCard DevilsMirk
devilsMirk = location DevilsMirk Cards.devilsMirk

-- | Immune to player card effects.
-- While Devil's Mirk is in the staging area, characters cannot be healed.
-- While Devil's Mirk is the active location, each Wose enemy gets +2 defense.
instance HasModifiersFor DevilsMirk where
  getModifiersFor (DevilsMirk a) = do
    modified a a [ImmuneToPlayerCardEffects]
    when (a.placement == StagingArea) do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> modified a c [CannotBeHealed]
    when (a.placement == ActiveLocation) do
      woses <- select (EnemyWithTrait Wose)
      for_ woses \w -> modified a w [DefenseModifier 2]

instance RunMessage DevilsMirk where
  runMessage msg (DevilsMirk a) = DevilsMirk <$> runMessage msg a
