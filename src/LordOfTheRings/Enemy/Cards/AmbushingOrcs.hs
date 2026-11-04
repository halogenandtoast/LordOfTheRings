module LordOfTheRings.Enemy.Cards.AmbushingOrcs (ambushingOrcs) where

import LordOfTheRings.Card (CardCode, toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGuard)

newtype AmbushingOrcs = AmbushingOrcs EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ambushingOrcs :: EnemyCard AmbushingOrcs
ambushingOrcs = enemy AmbushingOrcs Cards.ambushingOrcs

{- | The shadow cards of the scenario's encounter sets that target the defending player:
dealt for an attack on the player guarding Gollum, they target him. Those that name the
player guarding Gollum reach each player through
'LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers.shadowGuardTargets'; those
that touch only the attack or a defending character target no player.
-}
defendingPlayerShadows :: [CardCode]
defendingPlayerShadows = map toCardCode [Cards.kingSpider, Cards.hummerhorns, Cards.ungoliantsSpawn]

-- | When Ambushing Orcs is dealt a shadow card that targets the player guarding Gollum,
-- that shadow card also targets each other player: its effect on the defending player
-- resolves again for each other, for the same attack, its defenders the same.
instance RunMessage AmbushingOrcs where
  runMessage msg (AmbushingOrcs attrs) = case msg of
    ResolveShadowCard cid eid pid defenders | eid == attrs.id -> do
      guarding <- (== Just pid) <$> getGuard
      let targetsPlayer = any (\c -> c.id == cid && toCardCode c `elem` defendingPlayerShadows) attrs.shadowCards
      when (guarding && targetsPlayer) do
        others <- filter (/= pid) <$> getRemainingPlayers
        for_ others \p -> push $ ResolveShadowCard cid eid p defenders
      AmbushingOrcs <$> runMessage msg attrs
    _ -> AmbushingOrcs <$> runMessage msg attrs
