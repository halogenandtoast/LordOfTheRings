module LordOfTheRings.Quest.Cards.LostInTheDark (lostInTheDark) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype LostInTheDark = LostInTheDark QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
lostInTheDark :: QuestCard LostInTheDark
lostInTheDark = quest LostInTheDark Cards.lostInTheDark

-- | Forced: After a player raises his threat by any amount, he discards the top card from
-- under his threat dial.
instance HasAbilities LostInTheDark where
  getAbilities (LostInTheDark a) = [mkAbility a 1 $ forced $ ThreatRaisedWindow After AnyPlayer]

-- | When Revealed: Each player places the top 10 cards of his deck facedown under his
-- threat dial (without looking at them). When this stage is defeated, each player shuffles
-- all cards under his threat dial back into his deck.
instance RunMessage LostInTheDark where
  runMessage msg q@(LostInTheDark attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer (`placeUnderThreatDial` 10)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (ThreatRaised pid) <- ws] discardFromUnderThreatDial
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer shuffleUnderThreatDialIntoDeck
      LostInTheDark <$> runMessage msg attrs
    _ -> LostInTheDark <$> runMessage msg attrs
