module LordOfTheRings.Location.Cards.UmbarBridge (umbarBridge) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)
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
-- What it has canceled this round counts toward the X.
instance HasModifiersFor UmbarBridge where
  getModifiersFor (UmbarBridge a) = when (a.placement == StagingArea) do
    raiders <- traverse getEnemy =<< select (EnemyWithTrait Raider)
    x <- foldr max 0 <$> traverse (\r -> resourcesHeld r r.resources) raiders
    when (x > 0) $ getCurrentQuest >>= traverse_ \q -> do
      effects <- getsGame gameEffects
      let canceled = sum [k | e <- effects, effectTarget e == q, ProgressCanceledThisRound k <- effectModifiers e]
      when (x > canceled) $ modified a q [ProgressReducedBy (x - canceled)]

-- | Travel: Reduce the engagement cost of each Corsair enemy to 0 until the end of the
-- round.
instance RunMessage UmbarBridge where
  runMessage msg (UmbarBridge a) = case msg of
    TravelTo lid | lid == a.id -> do
      corsairs <- traverse getEnemy =<< select (EnemyWithTrait Corsair)
      for_ corsairs \e -> roundModifier a e.id (EngagementCostModifier (negate e.engagementCost))
      UmbarBridge <$> runMessage msg a
    _ -> UmbarBridge <$> runMessage msg a
