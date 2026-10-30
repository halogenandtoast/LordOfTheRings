module LordOfTheRings.Ally.Cards.RobinSmallburrow (robinSmallburrow) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Helpers.Staging (getLocationQuestPoints)
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype RobinSmallburrow = RobinSmallburrow AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

robinSmallburrow :: AllyCard RobinSmallburrow
robinSmallburrow = ally RobinSmallburrow Cards.robinSmallburrow

-- | Response: After you travel to a location, spend 1 spirit resource to raise the
-- engagement cost of each enemy in play by X until the end of the round, where X is that
-- location's quest points. Any player may trigger this response.
instance HasAbilities RobinSmallburrow where
  getAbilities (RobinSmallburrow a) =
    [anyPlayer $ mkAbility a 1 $ ResponseAbility (AnyTraveledWindow After) (SpendResourcesCost Spirit 1)]

instance RunMessage RobinSmallburrow where
  runMessage msg s@(RobinSmallburrow a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (take 1 [lid | Window _ (Traveled lid) <- windows]) \lid -> do
        x <- getLocationQuestPoints lid
        enemies <- select AnyEnemy
        for_ enemies \e -> roundModifier a e (EngagementCostModifier x)
      pure s
    _ -> RobinSmallburrow <$> runMessage msg a
