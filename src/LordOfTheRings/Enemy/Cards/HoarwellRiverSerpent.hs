module LordOfTheRings.Enemy.Cards.HoarwellRiverSerpent (hoarwellRiverSerpent) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Safe))
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers (damagedCharactersOf, isSafe)

newtype HoarwellRiverSerpent = HoarwellRiverSerpent EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

hoarwellRiverSerpent :: EnemyCard HoarwellRiverSerpent
hoarwellRiverSerpent = enemy HoarwellRiverSerpent Cards.hoarwellRiverSerpent

-- | Forced: When you engage Hoarwell River Serpent, deal 1 damage to each damaged character
-- you control.
instance HasAbilities HoarwellRiverSerpent where
  getAbilities (HoarwellRiverSerpent a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Who could engage it to pay the cost: any player while it is in the staging area, else
-- a player it is not engaged with.
engagers :: HasGame m => EnemyAttrs -> m [PlayerId]
engagers a = case a.placement of
  EngagedWith pid -> filter (/= pid) <$> getRemainingPlayers
  _ -> getRemainingPlayers

-- | As an additional cost to travel to a safe location, one player must engage Hoarwell
-- River Serpent. With no one able to, the cost cannot be paid.
instance HasModifiersFor HoarwellRiverSerpent where
  getModifiersFor (HoarwellRiverSerpent a) = do
    none <- null <$> engagers a
    when none do
      safe <- select (LocationInStagingArea <> LocationWithKeyword Safe)
      for_ safe \l -> modified a l [CannotBeTraveledTo]

-- | Paid as a Travel cost is, before the location becomes active; the first player chooses
-- who engages it, for the group.
instance RunMessage HoarwellRiverSerpent where
  runMessage msg e@(HoarwellRiverSerpent attrs) = case msg of
    TravelTo lid -> do
      safe <- isSafe lid
      when safe do
        lead <- getLead
        players <- engagers attrs
        chooseOrRunOneM lead do
          questionLabeled "choosePlayer"
          causedBy attrs
          targets players \pid -> engageEnemy pid attrs.id
      HoarwellRiverSerpent <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        damaged <- damagedCharactersOf pid
        for_ damaged \c -> dealDamage attrs c 1
      pure e
    _ -> HoarwellRiverSerpent <$> runMessage msg attrs
