{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Ally.Runner (module X) where

import LordOfTheRings.Ally.Types as X
import LordOfTheRings.Classes.HasQueue (HasQueue, push, pushAll, pushWhen)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Helpers.Modifiers (doesNotExhaustToCommit, getModifiers)
import LordOfTheRings.Message
import LordOfTheRings.Modifier (ModifierType (CannotExhaust, CannotReady, DoesNotExhaustToAttack))
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window (WindowType (BecameReady, Exhausted, RemovedFromQuest), mkAfter)

instance RunMessage AllyAttrs where
  runMessage msg attrs = case msg of
    Exhaust (isTarget attrs -> True) -> do
      blocked <- elem CannotExhaust <$> getModifiers attrs
      exhausting (not blocked) attrs
    AttackWith (isTarget attrs -> True) -> do
      spared <- elem DoesNotExhaustToAttack <$> getModifiers attrs
      (attackingL .~ True) <$> exhausting (not spared) attrs
    StopAttacking (isTarget attrs -> True) -> pure $ attrs & attackingL .~ False
    CommitToQuest (isTarget attrs -> True) -> do
      free <- doesNotExhaustToCommit (toTarget attrs)
      (committedL .~ True) <$> exhausting (not free) attrs
    -- an ally put into play committed is aimed at by its card
    CommitToQuest (CardIdTarget cid) | cid == attrs.allyCardId -> pure $ attrs & exhaustedL .~ True & committedL .~ True
    EndPhase QuestPhase -> pure $ attrs & committedL .~ False
    RemoveFromQuest (isTarget attrs -> True) -> do
      pushWhen attrs.allyCommitted $ CheckWindows [mkAfter $ RemovedFromQuest (toTarget attrs)]
      pure $ attrs & committedL .~ False
    Ready (isTarget attrs -> True) -> do
      blocked <- elem CannotReady <$> getModifiers attrs
      when (attrs.allyExhausted && not blocked) $ push $ CheckWindows [mkAfter $ BecameReady (toTarget attrs)]
      pure $ attrs & exhaustedL %~ (&& blocked)
    AttachFacedown (isTarget attrs -> True) card -> pure $ attrs & facedownL %~ (card :)
    PlaceAlly aid placement | aid == attrs.allyId -> pure $ attrs & placementL .~ placement
    -- the response of an ally in hand, as Brok Ironfist's, puts it into play
    UseHandAction pid cid | cid == attrs.allyCardId, isJust attrs.allyHandWindow -> do
      push $ PutAllyIntoPlay pid cid
      pure attrs
    -- answering its discard window, it is put into play from the discard pile
    RespondFromDiscard pid card _ | card.id == attrs.allyCardId, isJust attrs.allyDiscardWindow -> do
      push $ RemoveFromDiscard pid card.id
      push $ PutAllyIntoPlayUnder pid card
      pure attrs
    -- the cards facedown under it go to their owners' discard piles as it leaves play
    Discard (isTarget attrs -> True) -> leaving
    ReturnToHand (isTarget attrs -> True) -> leaving
    ShuffleIntoOwnersDeck (isTarget attrs -> True) -> leaving
    PutOnBottomOfOwnersDeck (isTarget attrs -> True) -> leaving
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
   where
    leaving = do
      pushAll $ map PutInDiscard attrs.allyFacedown
      pure $ attrs & facedownL .~ []

-- | The character exhausts, when it does, opening the window for responses to it exhausting.
exhausting :: HasQueue Message m => Bool -> AllyAttrs -> m AllyAttrs
exhausting exhausts attrs
  | exhausts && not attrs.allyExhausted = do
      push $ CheckWindows [mkAfter $ Exhausted (toTarget attrs)]
      pure $ attrs & exhaustedL .~ True
  | otherwise = pure attrs
