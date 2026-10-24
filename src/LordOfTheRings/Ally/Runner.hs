{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Ally.Runner (module X) where

import LordOfTheRings.Ally.Types as X
import LordOfTheRings.Card.CardDef (HasCardDef (..))
import LordOfTheRings.Classes.HasQueue (pushAll)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Helpers.Modifiers (doesNotExhaustToCommit, getModifiers)
import LordOfTheRings.Message
import LordOfTheRings.Modifier (ModifierType (DoesNotExhaustToAttack))
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Token

instance RunMessage AllyAttrs where
  runMessage msg attrs = case msg of
    Exhaust (isTarget attrs -> True) -> pure $ attrs & exhaustedL .~ True
    AttackWith (isTarget attrs -> True) -> do
      spared <- elem DoesNotExhaustToAttack <$> getModifiers attrs
      pure $ attrs & exhaustedL %~ (|| not spared) & attackingL .~ True
    StopAttacking (isTarget attrs -> True) -> pure $ attrs & attackingL .~ False
    CommitToQuest (isTarget attrs -> True) -> do
      free <- doesNotExhaustToCommit (toTarget attrs) (toCardDef attrs)
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
