module LordOfTheRings.Quest.Runner (module LordOfTheRings.Quest.Runner, module X) where

import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Quest.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token

instance RunMessage QuestAttrs where
  runMessage msg attrs = case msg of
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      let attrs' = attrs & tokensL %~ addTokens token n
      when (token == Progress && attrs'.questPoints > 0 && attrs'.progress >= attrs'.questPoints)
        $ push (CompleteQuest attrs.id)
      pure attrs'
    Flip (isTarget attrs -> True) -> pure $ attrs & flippedL .~ True
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
