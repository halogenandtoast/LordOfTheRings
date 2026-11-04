module LordOfTheRings.Enemy.Cards.HoarwellRiverSerpent (hoarwellRiverSerpent) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers (damagedCharactersOf, isSafe)

newtype HoarwellRiverSerpent = HoarwellRiverSerpent EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hoarwellRiverSerpent :: EnemyCard HoarwellRiverSerpent
hoarwellRiverSerpent = enemy HoarwellRiverSerpent Cards.hoarwellRiverSerpent

-- | Forced: When you engage Hoarwell River Serpent, deal 1 damage to each damaged character
-- you control.
instance HasAbilities HoarwellRiverSerpent where
  getAbilities (HoarwellRiverSerpent a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | As an additional cost to travel to a safe location, one player must engage Hoarwell
-- River Serpent.
instance RunMessage HoarwellRiverSerpent where
  runMessage msg e@(HoarwellRiverSerpent attrs) = case msg of
    TravelTo lid -> do
      safe <- isSafe lid
      when (safe && attrs.placement == StagingArea) do
        lead <- getLead
        engageEnemy lead attrs.id
      HoarwellRiverSerpent <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        damaged <- damagedCharactersOf pid
        for_ damaged \c -> dealDamage attrs c 1
      pure e
    _ -> HoarwellRiverSerpent <$> runMessage msg attrs
