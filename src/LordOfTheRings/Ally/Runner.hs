module LordOfTheRings.Ally.Runner (module LordOfTheRings.Ally.Runner, module X) where

import LordOfTheRings.Classes.HasQueue (pushAll)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Ally.Types as X
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Message
import LordOfTheRings.Modifier (ModifierType (DoesNotExhaustToCommit))
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token

instance RunMessage AllyAttrs where
  runMessage msg attrs = case msg of
    Exhaust (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ True
    AttackWith (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ True & attackingL .~ True
    StopAttacking (isTarget attrs -> True) -> pure $ attrs & attackingL .~ False
    CommitToQuest (isTarget attrs -> True) -> do
      free <- elem DoesNotExhaustToCommit <$> getModifiers attrs
      pure $ attrs & exhaustedL %~ (|| not free) & committedL .~ True
    -- an ally put into play committed is aimed at by its card
    CommitToQuest (CardIdTarget cid) | cid == attrs.allyCardId -> pure $ attrs & exhaustedL .~ True & committedL .~ True
    EndPhase QuestPhase -> pure $ attrs & committedL .~ False
    RemoveFromQuest (isTarget attrs -> True) -> pure $ attrs & committedL .~ False
    Ready (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ False
    AttachFacedown aid card | aid == attrs.allyId -> pure $ attrs & facedownL %~ (card :)
    -- the cards facedown under it go to their owners' discard piles as it leaves play
    Discard (isTarget attrs -> True) -> leaving
    ReturnToHand (isTarget attrs -> True) -> leaving
    ShuffleIntoOwnersDeck (isTarget attrs -> True) -> leaving
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ addTokens token n
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
   where
    leaving = do
      pushAll $ map PutInDiscard attrs.allyFacedown
      pure $ attrs & facedownL .~ []
