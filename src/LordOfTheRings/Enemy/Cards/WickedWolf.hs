module LordOfTheRings.Enemy.Cards.WickedWolf (wickedWolf) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim.Helpers (hasRiddle)

newtype WickedWolf = WickedWolf EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wickedWolf :: EnemyCard WickedWolf
wickedWolf = enemy WickedWolf Cards.wickedWolf

instance RunMessage WickedWolf where
  runMessage msg e@(WickedWolf attrs) = case msg of
    CheckWindows windows -> do
      -- The card is now on the enemy, but has not been revealed or resolved yet.
      for_
        [ pid
        | Window After (ShadowCardDealt eid cid pid) <- windows
        , eid == attrs.id
        , c <- attrs.shadowCards
        , c.id == cid
        , hasRiddle c
        ]
        \pid ->
          chooseAndExhaust attrs pid 1
      WickedWolf <$> runMessage msg attrs
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure e
    _ -> WickedWolf <$> runMessage msg attrs
