module LordOfTheRings.Hero.Runner (module LordOfTheRings.Hero.Runner, module X) where

import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Hero.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token

instance RunMessage HeroAttrs where
  runMessage msg attrs = case msg of
    BeginPhase ResourcePhase -> do
      push $ PlaceTokens GameSource (toTarget attrs) Resource 1
      pure attrs
    Exhaust (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ True
    AttackWith (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ True & attackingL .~ True
    StopAttacking (isTarget attrs -> True) -> pure $ attrs & attackingL .~ False
    CommitToQuest (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ True & committedL .~ True
    EndPhase QuestPhase -> pure $ attrs & committedL .~ False
    Ready (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ False
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
