{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Hero.Runner (module X) where

import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Helpers.Modifiers (doesNotExhaustToCommit, getModifiers)
import LordOfTheRings.Hero.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Modifier (ModifierType (CannotCollectResources, CannotExhaust, CannotGainResourcesFromCardEffects, CannotGainResourcesFromPlayerCardEffects, CannotReady, CollectsAdditionalResources, DoesNotExhaustToAttack, SpentResourcesThisRound))
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
      unless (attrs.heroPrisoner || CannotCollectResources `elem` mods) $ push $ PlaceTokens GameSource (toTarget attrs) Resource collected
      pure attrs
    TakePrisoner hid | hid == attrs.heroId -> pure $ attrs & prisonerL .~ True
    RescuePrisoner hid | hid == attrs.heroId -> do
      push $ PlaceTokens GameSource (toTarget attrs) Damage 1
      pure $ attrs & prisonerL .~ False
    PlaceTokens _ (isTarget attrs -> True) Damage _ | attrs.heroPrisoner -> pure attrs
    GuardCharacter (isTarget attrs -> True) cid -> pure $ attrs & guardedByL ?~ cid & committedL .~ False & attackingL .~ False
    RescueCharacter (isTarget attrs -> True) -> pure $ attrs & guardedByL .~ Nothing & exhaustedL .~ True
    -- the resource phase's resources come from the game, not a card effect
    PlaceTokens source (isTarget attrs -> True) Resource n | source /= GameSource -> do
      mods <- getModifiers attrs
      let barred = CannotGainResourcesFromCardEffects `elem` mods || (isPlayerCardSource source && CannotGainResourcesFromPlayerCardEffects `elem` mods)
      pure $ if barred then attrs else attrs & tokensL %~ addTokens Resource n
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
    EndPhase QuestPhase -> pure $ attrs & committedL .~ False
    RemoveFromQuest (isTarget attrs -> True) -> do
      pushWhen attrs.heroCommitted $ CheckWindows [mkAfter $ RemovedFromQuest (toTarget attrs)]
      pure $ attrs & committedL .~ False
    Ready (isTarget attrs -> True) -> do
      -- on the game, no character readies, as Enfeebled's "exhausted characters cannot ready"
      blocked <- elem CannotReady . concat <$> sequence [getModifiers attrs, getModifiers GameTarget]
      when (attrs.heroExhausted && not blocked) $ push $ CheckWindows [mkAfter $ BecameReady (toTarget attrs)]
      pure $ attrs & exhaustedL %~ (&& blocked)
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    -- a player spending resources from its pool, as Tighten Our Belts reads
    RemoveTokens (PlayerSource _) (isTarget attrs -> True) Resource n | n > 0 && attrs.resources > 0 -> do
      push $ CreateEffect $ Effect GameSource (toTarget attrs) [SpentResourcesThisRound] UntilEndOfRound
      pure $ attrs & tokensL %~ subtractTokens Resource n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs

-- | The character exhausts, when it does, opening the window for responses to it exhausting.
exhausting :: HasQueue Message m => Bool -> HeroAttrs -> m HeroAttrs
exhausting exhausts attrs
  | exhausts && not attrs.heroExhausted = do
      push $ CheckWindows [mkAfter $ Exhausted (toTarget attrs)]
      pure $ attrs & exhaustedL .~ True
  | otherwise = pure attrs
