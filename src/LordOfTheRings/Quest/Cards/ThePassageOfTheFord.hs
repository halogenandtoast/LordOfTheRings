module LordOfTheRings.Quest.Cards.ThePassageOfTheFord (thePassageOfTheFord) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ThePassageOfTheFord = ThePassageOfTheFord QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

thePassageOfTheFord :: QuestCard ThePassageOfTheFord
thePassageOfTheFord = quest ThePassageOfTheFord Cards.thePassageOfTheFord

-- | The players cannot travel to The Old Ford unless there is at least 5 progress here.
-- This stage cannot be defeated while The Old Ford is in play.
instance HasModifiersFor ThePassageOfTheFord where
  getModifiersFor (ThePassageOfTheFord a) = when a.flipped do
    fords <- select (LocationWithCardCode $ toCardCode Locations.theOldFordJourneyUpTheAnduin)
    when (a.progress < 5) $ for_ fords \l -> modified a l [CannotBeTraveledTo]
    unless (null fords) $ modifySelf a [CannotBeDefeated]

-- | Forced: At the beginning of the quest phase, add the top card of the Evil Creatures deck
-- to the staging area.
instance HasAbilities ThePassageOfTheFord where
  getAbilities (ThePassageOfTheFord a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase | a.flipped]

-- | When Revealed: The first player adds The Old Ford to the staging area. Each other player
-- takes the top card of the Evil Creatures deck and adds it to the staging area.
-- When The Old Ford is explored, the players win the game.
instance RunMessage ThePassageOfTheFord where
  runMessage msg q@(ThePassageOfTheFord attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      ThePassageOfTheFord <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.theOldFordJourneyUpTheAnduin)
      lead <- getLead
      others <- filter (/= lead) <$> getRemainingPlayers
      for_ others \_ -> addFromEvilCreaturesDeck attrs
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      addFromEvilCreaturesDeck attrs
      pure q
    CheckWindows ws | attrs.flipped, any (cardMatches (cardIs Locations.theOldFordJourneyUpTheAnduin)) [c | Window After (ExploredCard c) <- ws] -> do
      winGame
      ThePassageOfTheFord <$> runMessage msg attrs
    _ -> ThePassageOfTheFord <$> runMessage msg attrs
