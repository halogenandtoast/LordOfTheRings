module LordOfTheRings.Enemy.Cards.AmbushingOrcs (ambushingOrcs) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGuard)

newtype AmbushingOrcs = AmbushingOrcs EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ambushingOrcs :: EnemyCard AmbushingOrcs
ambushingOrcs = enemy AmbushingOrcs Cards.ambushingOrcs

-- | When Ambushing Orcs is dealt a shadow card that targets the player guarding Gollum,
-- that shadow card also targets each other player.
-- ponytail: the effect resolves again against each other player, as an undefended attack
instance RunMessage AmbushingOrcs where
  runMessage msg (AmbushingOrcs attrs) = case msg of
    ResolveShadowCard cid eid pid _ | eid == attrs.id -> do
      guarding <- (== Just pid) <$> getGuard
      when guarding do
        others <- filter (/= pid) <$> getRemainingPlayers
        for_ others \p -> push $ ResolveShadowCard cid eid p []
      AmbushingOrcs <$> runMessage msg attrs
    _ -> AmbushingOrcs <$> runMessage msg attrs
