{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Hero.Runner (module X) where

import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Card (cdHitPoints, toCardDef)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Helpers.Modifiers (doesNotExhaustToCommit, getModifiers, resourcesBarredFrom)
import LordOfTheRings.Hero.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Modifier (ModifierType (CannotCollectResources, CannotExhaust, CannotReady, CannotReadyFromCardEffects, CollectsAdditionalResources, ControlledCardsCannotReady, DoesNotExhaustToAttack, ReadiedByRefresh, SpentResourcesThisRound))
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window (WindowType (BecameReady, Exhausted, RemovedFromQuest), mkAfter)

instance RunMessage HeroAttrs where
  runMessage msg attrs = case msg of
    DoPhase ResourcePhase -> do
      mods <- getModifiers attrs
      let collected = 1 + sum [n | CollectsAdditionalResources n <- mods]
      unless (attrs.heroPrisoner || attrs.heroInStagingArea || CannotCollectResources `elem` mods) $ push $ PlaceTokens GameSource (toTarget attrs) Resource collected
      pure attrs
    TakePrisoner hid | hid == attrs.heroId -> pure $ attrs & prisonerL .~ True
    AttachFacedown (isTarget attrs -> True) card -> pure $ attrs & facedownL %~ (card :)
    AttachPoison (isTarget attrs -> True) cards -> do
      let poison = attrs.heroPoison <> cards
      -- as much poison as its printed hit points
      pushWhen (maybe False (length poison >=) (cdHitPoints $ toCardDef attrs)) $ MakeUnconscious (toTarget attrs)
      pure $ attrs & poisonL .~ poison
    DiscardPoison (isTarget attrs -> True) n -> do
      let (gone, kept) = splitAt n attrs.heroPoison
      pushAll $ map PutInDiscard gone
      pure $ attrs & poisonL .~ kept
    -- a poison card discarded as a Condition attachment, as Miner of the Iron Hills does
    Discard _ (CardIdTarget cid) | Just pc <- find ((== cid) . (.id)) attrs.heroPoison -> do
      push $ PutInDiscard pc
      pure $ attrs & poisonL %~ filter ((/= cid) . (.id))
    MakeUnconscious (isTarget attrs -> True) -> do
      pushAll $ map PutInDiscard attrs.heroPoison
      pure $ attrs & poisonL .~ [] & unconsciousL .~ True & exhaustedL .~ True
    ReadyUnconscious (isTarget attrs -> True) | attrs.heroUnconscious -> do
      push $ CheckWindows [mkAfter $ BecameReady (toTarget attrs)]
      pure $ attrs & unconsciousL .~ False & exhaustedL .~ False
    ReturnFacedownToHand (isTarget attrs -> True) cid -> do
      pushAll [PutInHand c | c <- attrs.heroFacedown, c.id == cid]
      pure $ attrs & facedownL %~ filter ((/= cid) . (.id))
    -- the cards facedown under it go to their owners' discard piles as it leaves play
    Discard _ (isTarget attrs -> True) -> leaving
    ReturnToHand (isTarget attrs -> True) -> leaving
    ShuffleIntoOwnersDeck (isTarget attrs -> True) -> leaving
    PutOnBottomOfOwnersDeck (isTarget attrs -> True) -> leaving
    RescuePrisoner hid | hid == attrs.heroId -> do
      push $ PlaceTokens GameSource (toTarget attrs) Damage 1
      pure $ attrs & prisonerL .~ False
    PlaceTokens _ (isTarget attrs -> True) Damage _ | attrs.heroPrisoner -> pure attrs
    -- a captive is attached to its guard, out of the staging area
    GuardCharacter (isTarget attrs -> True) cid -> pure $ attrs & guardedByL ?~ cid & committedL .~ False & attackingL .~ False & inStagingAreaL .~ False
    -- free of encounters, it is no longer a guarded objective; the card that frees it says
    -- whether it is exhausted
    RescueCharacter (isTarget attrs -> True) -> pure $ attrs & guardedByL .~ Nothing
    MoveHeroToStagingArea hid | hid == attrs.heroId -> pure $ attrs & inStagingAreaL .~ True & committedL .~ False & attackingL .~ False
    TakeControlOfHero hid | hid == attrs.heroId -> pure $ attrs & inStagingAreaL .~ False & exhaustedL .~ True
    -- the resource phase's resources come from the game, not a card effect
    PlaceTokens source (isTarget attrs -> True) Resource n | source /= GameSource -> do
      barred <- resourcesBarredFrom source attrs
      pure $ if barred then attrs else attrs & tokensL %~ addTokens Resource n
    Exhaust (isTarget attrs -> True) -> do
      blocked <- elem CannotExhaust <$> getModifiers attrs
      exhausting (not blocked) attrs
    Do (Exhaust (isTarget attrs -> True)) -> pure $ attrs & exhaustedL .~ True
    AttackWith (isTarget attrs -> True) -> do
      spared <- elem DoesNotExhaustToAttack <$> getModifiers attrs
      (attackingL .~ True) <$> exhausting (not spared) attrs
    StopAttacking (isTarget attrs -> True) -> pure $ attrs & attackingL .~ False
    CommitToQuest (isTarget attrs -> True) -> do
      free <- doesNotExhaustToCommit (toTarget attrs)
      (committedL .~ True) <$> exhausting (not free) attrs
    EndPhase QuestPhase -> pure $ attrs & committedL .~ False
    RemoveFromQuest (isTarget attrs -> True) -> do
      pushWhen attrs.heroCommitted $ CheckWindows [mkAfter $ RemovedFromQuest (toTarget attrs)]
      pure $ attrs & committedL .~ False
    Ready (isTarget attrs -> True) -> do
      -- on the game, no character readies, as Enfeebled's "exhausted characters cannot ready";
      -- nor one whose controller's cards cannot, as Ungoliant's Brood's; a card effect cannot
      -- ready one barred from it, the refresh phase still does
      mods <- getModifiers attrs
      held <- elem ControlledCardsCannotReady <$> getModifiers (PlayerTarget attrs.owner)
      blocked <- (|| held || (CannotReadyFromCardEffects `elem` mods && ReadiedByRefresh `notElem` mods)) . elem CannotReady . (mods <>) <$> getModifiers GameTarget
      when (attrs.heroExhausted && not blocked) $ push $ CheckWindows [mkAfter $ BecameReady (toTarget attrs)]
      pure $ attrs & exhaustedL %~ (&& blocked) & unconsciousL %~ (&& blocked)
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    -- a player spending resources from its pool, as Tighten Our Belts reads
    RemoveTokens (PlayerSource _) (isTarget attrs -> True) Resource n | n > 0 && attrs.resources > 0 -> do
      push $ CreateEffect $ Effect GameSource (toTarget attrs) [SpentResourcesThisRound] UntilEndOfRound
      pure $ attrs & tokensL %~ subtractTokens Resource n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
   where
    leaving = do
      pushAll $ map PutInDiscard (attrs.heroFacedown <> attrs.heroPoison)
      pure $ attrs & facedownL .~ [] & poisonL .~ []

-- | The character exhausts, its When and After windows framing it.
exhausting :: HasQueue Message m => Bool -> HeroAttrs -> m HeroAttrs
exhausting exhausts attrs = do
  when (exhausts && not attrs.heroExhausted) $ pushAll $ doFrame (Exhaust $ toTarget attrs) (Exhausted $ toTarget attrs)
  pure attrs
