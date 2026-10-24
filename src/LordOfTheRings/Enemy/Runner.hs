{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Enemy.Runner (module X) where

import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Enemy.Types as X
import LordOfTheRings.Message
import LordOfTheRings.I18n (ikey)
import LordOfTheRings.Phase
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Question
import LordOfTheRings.Source (sourceTarget)
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window (WindowType (ShadowCardDealt), mkAfter)

instance RunMessage EnemyAttrs where
  runMessage msg attrs = case msg of
    PlaceEnemy eid placement | eid == attrs.id -> pure $ attrs & placementL .~ placement
    AttachShadowCard eid card | eid == attrs.id -> do
      for_ (placementController attrs.placement) \pid ->
        push $ CheckWindows [mkAfter $ ShadowCardDealt eid card.id pid]
      pure $ attrs & shadowCardsL %~ (<> [card])
    LookAtShadowCard pid source eid cid | eid == attrs.id -> do
      for_ (find ((== cid) . (.id)) attrs.shadowCards) \card ->
        push
          $ Ask pid
          $ QuestionLabel (ikey "label.shadowCard") (sourceTarget source) []
          $ SearchCards [EncounterCard card]
          $ ChooseOne [Label (ikey "label.continue") []]
      pure attrs
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
