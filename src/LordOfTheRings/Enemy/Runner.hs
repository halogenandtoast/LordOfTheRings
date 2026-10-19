module LordOfTheRings.Enemy.Runner (module LordOfTheRings.Enemy.Runner, module X) where

import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Phase
import LordOfTheRings.Enemy.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token

instance RunMessage EnemyAttrs where
  runMessage msg attrs = case msg of
    PlaceEnemy eid placement | eid == attrs.id -> pure $ attrs & placementL .~ placement
    AttachShadowCard eid card | eid == attrs.id -> pure $ attrs & shadowCardsL %~ (<> [card])
    RemoveShadowCard eid cid | eid == attrs.id -> pure $ attrs & shadowCardsL %~ filter ((/= cid) . (.id))
    RevealShadowCard eid cid | eid == attrs.id -> pure $ attrs & shadowRevealedL %~ (cid :)
    EndPhase CombatPhase | notNull attrs.shadowCards -> do
      push $ AddToEncounterDiscard attrs.shadowCards
      pure $ attrs & shadowCardsL .~ [] & shadowRevealedL .~ []
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
