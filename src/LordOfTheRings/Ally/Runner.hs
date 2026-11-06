{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Ally.Runner (module X) where

import LordOfTheRings.Ally.Types as X
import LordOfTheRings.Classes.HasQueue (HasQueue, push, pushAll, pushWhen)
import LordOfTheRings.Card (cdHitPoints, toCardDef)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Helpers.Modifiers (doesNotExhaustToCommit, getModifiers)
import LordOfTheRings.Message
import LordOfTheRings.Modifier (ModifierType (CannotAttack, CannotAttackOrDefend, CannotCommitToQuest, CannotExhaust, CannotReady, CannotReadyFromCardEffects, ControlledCardsCannotReady, DoesNotExhaustToAttack, ReadiedByRefresh))
import LordOfTheRings.Phase
import LordOfTheRings.Placement (Placement (AttachedTo, InPlayArea), placementController)
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window (WindowType (BecameReady, ExhaustedToAttack, Exhausted, RemovedFromQuest), mkWhen, mkAfter)

instance RunMessage AllyAttrs where
  runMessage msg attrs = if attrs.allyAsHero then pure attrs else case msg of
    Exhaust (isTarget attrs -> True) -> do
      blocked <- elem CannotExhaust <$> getModifiers attrs
      exhausting (not blocked) attrs
    Do (Exhaust (isTarget attrs -> True)) -> pure $ attrs & exhaustedL .~ True
    AttackWithAgainst (isTarget attrs -> True) eid -> attackAgainst False eid
    ExhaustToAttack (isTarget attrs -> True) eid -> attackAgainst True eid
    AttackWith (isTarget attrs -> True) -> do
      mods <- getModifiers attrs
      if any (`elem` mods) [CannotAttack, CannotAttackOrDefend]
        then pure attrs
        else (attackingL .~ True) <$> exhausting (DoesNotExhaustToAttack `notElem` mods) attrs
    StopAttacking (isTarget attrs -> True) -> pure $ attrs & attackingL .~ False
    CommitToQuest (isTarget attrs -> True) -> do
      blocked <- elem CannotCommitToQuest <$> getModifiers attrs
      if blocked then pure attrs else do
        free <- doesNotExhaustToCommit (toTarget attrs)
        (committedL .~ True) <$> exhausting (not free) attrs
    -- an ally put into play committed is aimed at by its card
    CommitToQuest (CardIdTarget cid) | cid == attrs.allyCardId -> do
      blocked <- elem CannotCommitToQuest <$> getModifiers attrs
      pure $ if blocked then attrs else attrs & exhaustedL .~ True & committedL .~ True
    EndPhase QuestPhase -> pure $ attrs & committedL .~ False
    RemoveFromQuest (isTarget attrs -> True) -> do
      pushWhen attrs.allyCommitted $ CheckWindows [mkAfter $ RemovedFromQuest (toTarget attrs)]
      pure $ attrs & committedL .~ False
    Ready (isTarget attrs -> True) -> do
      -- on the game, no character readies, as Enfeebled's "exhausted characters cannot ready";
      -- nor one whose controller's cards cannot, as Ungoliant's Brood's; a card effect cannot
      -- ready one barred from it, the refresh phase still does
      held <- maybe (pure False) (fmap (elem ControlledCardsCannotReady) . getModifiers . PlayerTarget) (placementController attrs.placement)
      mods <- getModifiers attrs
      blocked <- (|| held || (CannotReadyFromCardEffects `elem` mods && ReadiedByRefresh `notElem` mods)) . elem CannotReady . (mods <>) <$> getModifiers GameTarget
      when (attrs.allyExhausted && not blocked) $ push $ CheckWindows [mkAfter $ BecameReady (toTarget attrs)]
      pure $ attrs & exhaustedL %~ (&& blocked) & unconsciousL %~ (&& blocked)
    AttachFacedown (isTarget attrs -> True) card -> pure $ attrs & facedownL %~ (card :)
    AttachPoison (isTarget attrs -> True) cards -> do
      let poison = attrs.allyPoison <> cards
      -- as much poison as its printed hit points
      pushWhen (maybe False (length poison >=) (cdHitPoints $ toCardDef attrs)) $ MakeUnconscious (toTarget attrs)
      pure $ attrs & poisonL .~ poison
    DiscardPoison (isTarget attrs -> True) n -> do
      let (gone, kept) = splitAt n attrs.allyPoison
      pushAll $ map PutInDiscard gone
      pure $ attrs & poisonL .~ kept
    -- a poison card discarded as a Condition attachment, as Miner of the Iron Hills does
    Discard _ (CardIdTarget cid) | Just pc <- find ((== cid) . (.id)) attrs.allyPoison -> do
      push $ PutInDiscard pc
      pure $ attrs & poisonL %~ filter ((/= cid) . (.id))
    MakeUnconscious (isTarget attrs -> True) -> do
      pushAll $ map PutInDiscard attrs.allyPoison
      pushWhen attrs.allyCommitted $ RemoveFromQuest (toTarget attrs)
      pure $ attrs & poisonL .~ [] & unconsciousL .~ True & exhaustedL .~ True & attackingL .~ False
    ReadyUnconscious (isTarget attrs -> True) | attrs.allyUnconscious -> do
      push $ CheckWindows [mkAfter $ BecameReady (toTarget attrs)]
      pure $ attrs & unconsciousL .~ False & exhaustedL .~ False
    ReturnFacedownToHand (isTarget attrs -> True) cid -> do
      pushAll [PutInHand c | c <- attrs.allyFacedown, c.id == cid]
      pure $ attrs & facedownL %~ filter ((/= cid) . (.id))
    PlaceAlly aid placement | aid == attrs.allyId -> pure $ attrs & placementL .~ placement
    GuardCharacter (isTarget attrs -> True) cid -> pure $ attrs & placementL .~ AttachedTo (CardIdTarget cid) & committedL .~ False
    RescueCharacter (isTarget attrs -> True) -> pure $ attrs & placementL .~ InPlayArea attrs.allyOwner & exhaustedL .~ True
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
    Discard _ (isTarget attrs -> True) -> leaving
    ReturnToHand (isTarget attrs -> True) -> leaving
    ShuffleIntoOwnersDeck (isTarget attrs -> True) -> leaving
    PutOnBottomOfOwnersDeck (isTarget attrs -> True) -> leaving
    RemoveFromGame (isTarget attrs -> True) -> leaving
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
   where
    attackAgainst forced eid = do
      mods <- getModifiers attrs
      if any (`elem` mods) [CannotAttack, CannotAttackOrDefend]
        then pure attrs
        else do
          when (CannotExhaust `notElem` mods && (forced || DoesNotExhaustToAttack `notElem` mods) && not attrs.allyExhausted) $ pushAll
            [ CheckWindows [mkWhen $ Exhausted (toTarget attrs), mkWhen $ ExhaustedToAttack (toTarget attrs) eid]
            , Do $ Exhaust (toTarget attrs)
            , CheckWindows [mkAfter $ Exhausted (toTarget attrs), mkAfter $ ExhaustedToAttack (toTarget attrs) eid]
            ]
          pure $ attrs & attackingL .~ True

    leaving = do
      pushAll $ map PutInDiscard (attrs.allyFacedown <> attrs.allyPoison)
      pure $ attrs & facedownL .~ [] & poisonL .~ []

-- | The character exhausts, its When and After windows framing it.
exhausting :: HasQueue Message m => Bool -> AllyAttrs -> m AllyAttrs
exhausting exhausts attrs = do
  when (exhausts && not attrs.allyExhausted) $ pushAll $ doFrame (Exhaust $ toTarget attrs) (Exhausted $ toTarget attrs)
  pure attrs
