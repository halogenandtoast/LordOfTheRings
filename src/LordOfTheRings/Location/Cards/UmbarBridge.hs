module LordOfTheRings.Location.Cards.UmbarBridge (umbarBridge) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Corsair, Raider))

newtype UmbarBridge = UmbarBridge LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

umbarBridge :: LocationCard UmbarBridge
umbarBridge = location UmbarBridge Cards.umbarBridge

-- | While Umbar Bridge is in the staging area, cancel the first X progress that would be
-- placed on the current quest each round, where X is the highest number of resources on a
-- Raider enemy in play.
instance HasModifiersFor UmbarBridge where
  getModifiersFor (UmbarBridge a) = when (a.placement == StagingArea) do
    raiders <- traverse getEnemy =<< select (EnemyWithTrait Raider)
    let x = foldr (max . (.resources)) 0 raiders
    when (x > 0) $ getCurrentQuest >>= traverse_ \q -> do
      placed <- sum . (\ms -> [k | ProgressPlacedThisRound k <- ms]) <$> getModifiers q
      when (x > placed) $ modified a q [ProgressReducedBy (x - placed)]

-- | Travel: Reduce the engagement cost of each Corsair enemy to 0 until the end of the
-- round.
instance RunMessage UmbarBridge where
  runMessage msg (UmbarBridge a) = case msg of
    TravelTo lid | lid == a.id -> do
      corsairs <- traverse getEnemy =<< select (EnemyWithTrait Corsair)
      for_ corsairs \e -> roundModifier a e.id (EngagementCostModifier (negate e.engagementCost))
      UmbarBridge <$> runMessage msg a
    _ -> UmbarBridge <$> runMessage msg a
