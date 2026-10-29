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
    RevealShadowCard eid cid | eid == attrs.id -> pure $ attrs & shadowRevealedL %~ (cid :) & shadowFaceUpL %~ filter (/= cid)
    TurnShadowCardFaceUp eid cid | eid == attrs.id -> pure $ attrs & shadowRevealedL %~ (cid :) & shadowFaceUpL %~ (cid :)
    EndPhase CombatPhase | notNull attrs.shadowCards -> discardShadows
    DiscardShadowCards eid | eid == attrs.id && notNull attrs.shadowCards -> discardShadows
    AttachFacedown (isTarget attrs -> True) card -> pure $ attrs & facedownL %~ (<> [card])
    -- the cards facedown under it go to their owners' discard piles as it leaves play
    Destroy _ (isTarget attrs -> True) -> leaving
    Discard (isTarget attrs -> True) -> leaving
    ReturnToTopOfEncounterDeck (isTarget attrs -> True) -> leaving
    ReturnToBottomOfEncounterDeck (isTarget attrs -> True) -> leaving
    ShuffleIntoEncounterDeck (isTarget attrs -> True) -> leaving
    RemoveFromGame (isTarget attrs -> True) -> leaving
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
   where
    leaving = do
      pushAll $ map PutInDiscard attrs.facedown
      pure $ attrs & facedownL .~ []
    discardShadows = do
      push $ AddToEncounterDiscard attrs.shadowCards
      pure $ attrs & shadowCardsL .~ [] & shadowRevealedL .~ [] & shadowFaceUpL .~ []
