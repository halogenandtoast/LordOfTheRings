module LordOfTheRings.Quest.Cards.TributeToMordor (tributeToMordor) where

import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TributeToMordor = TributeToMordor QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
tributeToMordor :: QuestCard TributeToMordor
tributeToMordor = quest TributeToMordor Cards.tributeToMordor

-- | This stage cannot be the active quest while Thane Ulchor has any hit points remaining.
instance HasModifiersFor TributeToMordor where
  getModifiersFor (TributeToMordor a) =
    whenM (selectAny $ thaneUlchor <> NotEnemy EnemyWithNoHitPointsRemaining) $ modifySelf a [CannotBeCurrentQuest]

instance HasAbilities TributeToMordor where
  getAbilities (TributeToMordor a) = [mkAbility a 1 $ forced RoundEndsWindow]

{- | Forced: At the end of the round, place 1 damage token here. Then, if there are 4 or
more damage tokens here, remove 4 of them and reveal the top card of the Power of Mordor
deck. If you cannot, the players lose the game.
When this stage is defeated, the players win the game.
-}
instance RunMessage TributeToMordor where
  runMessage msg q@(TributeToMordor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeTokens attrs attrs Damage 1
      doStep 1 msg
      pure q
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      when (countTokens Damage attrs.questTokens >= 4) do
        removeTokens attrs attrs Damage 4
        unlessM revealTopOfPowerOfMordorDeck (gameOver Lost)
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      winGame
      TributeToMordor <$> runMessage msg attrs
    _ -> TributeToMordor <$> runMessage msg attrs
