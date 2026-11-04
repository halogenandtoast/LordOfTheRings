module LordOfTheRings.Quest.Cards.TheLordOfMiddleEarth (theLordOfMiddleEarth) where

import LordOfTheRings.Card (CardCost (StaticCost), CardDef (cdCost), toCardDef)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Player.Types (discardableHand)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTempleOfDoom.Helpers

newtype TheLordOfMiddleEarth = TheLordOfMiddleEarth QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theLordOfMiddleEarth :: QuestCard TheLordOfMiddleEarth
theLordOfMiddleEarth = quest TheLordOfMiddleEarth Cards.theLordOfMiddleEarth

-- | This stage gets +4 quest points per player.
instance HasModifiersFor TheLordOfMiddleEarth where
  getModifiersFor (TheLordOfMiddleEarth a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [QuestPointModifier (4 * players)]
    stage2Modifiers a

instance HasAbilities TheLordOfMiddleEarth where
  getAbilities (TheLordOfMiddleEarth a) = stage2Forced a

{- | Forced: At the beginning of the quest phase, each player discards 1 card from his
hand. Until the end of the round, no more than X progress can be placed on this stage. X
is the total cost of each card discarded by this effect.

The players discard in turn order, the total so far carried as the step; a card without a
printed cost (X, or none) counts 0. Progress already placed this round does not count
against X. Its printed 0 quest points take the modifier's, as any stage's would: it is
defeated with as much progress as it has quest points once they are more than 0.
-}
instance RunMessage TheLordOfMiddleEarth where
  runMessage msg q@(TheLordOfMiddleEarth attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      players <- getRemainingPlayers
      for_ (take 1 players) \p -> doStep 0 (UseCardAbility p (toSource attrs) 1 ws)
      pure q
    DoStep x (UseCardAbility p (isSource attrs -> True) 1 ws) -> do
      players <- getRemainingPlayers
      player <- getPlayer p
      barred <- elem CannotDiscardFromHand <$> getModifiers (PlayerTarget p)
      let hand = if barred then [] else discardableHand player
          continue :: ReverseQueue m => Int -> m ()
          continue y = case drop 1 (dropWhile (/= p) players) of
            next : _ -> doStep y (UseCardAbility next (toSource attrs) 1 ws)
            [] -> do
              placed <- progressPlacedThisRound (toTarget attrs)
              roundModifier attrs attrs (ProgressPerRoundLimit $ placed + y)
      if null hand
        then continue x
        else chooseOneM p do
          questionLabeled "theLordOfMiddleEarth.discard"
          causedBy attrs
          targets hand \c -> do
            discardFromHand p c
            continue (x + cost c)
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress _ -> do
      attrs' <- runStage2 msg attrs
      defeatIfDone attrs'
      pure $ TheLordOfMiddleEarth attrs'
    CheckStage qid | qid == attrs.id -> do
      defeatIfDone attrs
      pure q
    _ -> TheLordOfMiddleEarth <$> runStage2 msg attrs
   where
    cost c = case cdCost (toCardDef c) of
      Just (StaticCost n) -> n
      _ -> 0
    defeatIfDone a = do
      points <- getQuestPoints a
      when (points > 0 && a.progress >= points) $ completeQuest a.id
