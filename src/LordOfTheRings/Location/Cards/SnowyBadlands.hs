module LordOfTheRings.Location.Cards.SnowyBadlands (snowyBadlands) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (isNight)

newtype SnowyBadlands = SnowyBadlands LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

snowyBadlands :: LocationCard SnowyBadlands
snowyBadlands = location SnowyBadlands Cards.snowyBadlands

-- | While Snowy Badlands is in the staging area, each enemy in the staging area cannot take
-- damage.
instance HasModifiersFor SnowyBadlands where
  getModifiersFor (SnowyBadlands a) = when (a.placement == StagingArea) do
    staged <- select EnemyInStagingArea
    for_ staged \e -> modified a e [CannotTakeDamage]

-- | Travel: Each player deals 1 damage to an exhausted character he controls to travel here
-- (2 damage instead if it is Night).
instance RunMessage SnowyBadlands where
  runMessage msg l@(SnowyBadlands a) = case msg of
    TravelTo lid | lid == a.id -> do
      n <- bool 1 2 <$> isNight
      eachPlayer \p -> do
        exhausted <- exhaustedCharactersOf p
        chooseOrRunOneM p do
          questionLabeled "snowyBadlands.damage"
          causedBy a
          targets exhausted \c -> dealDamage a c n
      pure l
    _ -> SnowyBadlands <$> runMessage msg a
