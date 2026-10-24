{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Hero.Runner (module X) where

import LordOfTheRings.Card.CardDef (HasCardDef (..))
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Helpers.Modifiers (doesNotExhaustToCommit, getModifiers)
import LordOfTheRings.Hero.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Modifier (ModifierType (CannotCollectResources, DoesNotExhaustToAttack))
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token

instance RunMessage HeroAttrs where
  runMessage msg attrs = case msg of
    BeginPhase ResourcePhase -> do
      blocked <- elem CannotCollectResources <$> getModifiers attrs
      unless (attrs.heroPrisoner || blocked) $ push $ PlaceTokens GameSource (toTarget attrs) Resource 1
      pure attrs
    TakePrisoner hid | hid == attrs.heroId -> pure $ attrs & prisonerL .~ True
    RescuePrisoner hid | hid == attrs.heroId -> do
      push $ PlaceTokens GameSource (toTarget attrs) Damage 1
      pure $ attrs & prisonerL .~ False
    PlaceTokens _ (isTarget attrs -> True) Damage _ | attrs.heroPrisoner -> pure attrs
    Exhaust (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ True
    AttackWith (isTarget attrs -> True) -> do
      spared <- elem DoesNotExhaustToAttack <$> getModifiers attrs
      pure $ attrs & exhaustedL %~ (|| not spared) & attackingL .~ True
    StopAttacking (isTarget attrs -> True) -> pure $ attrs & attackingL .~ False
    CommitToQuest (isTarget attrs -> True) -> do
      free <- doesNotExhaustToCommit (toTarget attrs) (toCardDef attrs)
      pure $ attrs & exhaustedL %~ (|| not free) & committedL .~ True
    EndPhase QuestPhase -> pure $ attrs & committedL .~ False
    RemoveFromQuest (isTarget attrs -> True) -> pure $ attrs & committedL .~ False
    Ready (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ False
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
