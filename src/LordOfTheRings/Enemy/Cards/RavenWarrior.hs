module LordOfTheRings.Enemy.Cards.RavenWarrior (ravenWarrior) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (removeTimeCountersFromLocations)

newtype RavenWarrior = RavenWarrior EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenWarrior :: EnemyCard RavenWarrior
ravenWarrior = enemy RavenWarrior Cards.ravenWarrior

-- | Forced: After Raven Warrior engages a player, remove X time counters from locations in
-- play. X is the number of cards in engaged player's hand.
instance HasAbilities RavenWarrior where
  getAbilities (RavenWarrior a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | The engaged player chooses the locations the counters come from.
-- Shadow: Defending player discards an attachment he controls.
instance RunMessage RavenWarrior where
  runMessage msg e@(RavenWarrior attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ | EngagedWith pid <- attrs.placement -> do
      hand <- length . (.hand) <$> getPlayer pid
      removeTimeCountersFromLocations attrs pid hand
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndDiscard attrs pid =<< getAttachmentsControlledBy pid
      pure e
    _ -> RavenWarrior <$> runMessage msg attrs
