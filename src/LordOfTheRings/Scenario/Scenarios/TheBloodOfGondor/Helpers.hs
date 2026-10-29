{- | What the cards of The Blood of Gondor share: the hidden cards, encounter cards
facedown in a player's play area (rules sheet).
-}
module LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (
  getHiddenCards,
  hiddenCardCount,
  takeHiddenCard,
  discardHiddenCard,
  turnFaceup,
  turnHiddenCardsFaceup,
  turnFaceupResolving,
) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (CardType (EnemyType), EncounterCard, cdCardType, cdWhenRevealed, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Id
import LordOfTheRings.Enemy.Types ()
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types ()
import LordOfTheRings.Target

-- | The hidden cards in the player's play area, the first taken first.
getHiddenCards :: HasGame m => PlayerId -> m [EncounterCard]
getHiddenCards pid = getsGame (Map.findWithDefault [] pid . (.hiddenCards) . toAttrs . gameScenario)

-- | The total number of hidden cards in the game, as Black Númenorean's X reads.
hiddenCardCount :: HasGame m => m Int
hiddenCardCount = getsGame (sum . map length . toList . (.hiddenCards) . toAttrs . gameScenario)

-- | The player takes 1 hidden card: the top card of the encounter deck, facedown.
takeHiddenCard :: ReverseQueue m => PlayerId -> m ()
takeHiddenCard = push . TakeHiddenCard

{- | The player discards 1 hidden card, as The Dark Woods reads.
ponytail: the first taken; they are facedown, so there is nothing to choose between
-}
discardHiddenCard :: ReverseQueue m => PlayerId -> m ()
discardHiddenCard pid = traverse_ (push . DiscardHiddenCard pid) . take 1 =<< getHiddenCards pid

{- | The hidden card turns faceup (rules sheet): an enemy engages the player, and is given
back so the caller can act on it once it has; a treachery or location is discarded.
-}
turnFaceup :: ReverseQueue m => PlayerId -> EncounterCard -> m (Maybe EnemyId)
turnFaceup pid card
  | cdCardType (toCardDef card) == EnemyType = do
      eid <- getRandom
      push $ TurnHiddenCardFaceup pid card [CreateEnemyEngaged pid eid card]
      pure $ Just eid
  | otherwise = Nothing <$ push (TurnHiddenCardFaceup pid card [AddToEncounterDiscard [card]])

{- | Each hidden card now in the player's play area turns faceup, one at a time; one taken
as they do stays facedown (FAQ). The enemies that engage the player are given back.
-}
turnHiddenCardsFaceup :: ReverseQueue m => PlayerId -> m [EnemyId]
turnHiddenCardsFaceup pid = catMaybes <$> (traverse (turnFaceup pid) =<< getHiddenCards pid)

{- | The hidden card turns faceup as Lying in Wait turns it: its When Revealed effects
resolve, a location going to the staging area; an enemy still engages the player.
-}
turnFaceupResolving :: ReverseQueue m => PlayerId -> EncounterCard -> m ()
turnFaceupResolving pid card
  | cdCardType def == EnemyType = do
      eid <- getRandom
      push
        $ TurnHiddenCardFaceup pid card
        $ CreateEnemyEngaged pid eid card
        : [AskToResolveWhenRevealed (EnemyTarget eid) card | cdWhenRevealed def]
  | otherwise = push $ TurnHiddenCardFaceup pid card [ResolveWhenRevealedOf card]
 where
  def = toCardDef card
