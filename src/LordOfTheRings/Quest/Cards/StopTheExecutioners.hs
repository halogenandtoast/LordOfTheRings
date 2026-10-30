module LordOfTheRings.Quest.Cards.StopTheExecutioners (stopTheExecutioners) where

import LordOfTheRings.Card (CardDef (cdCardType), CardType (AllyType, HeroType), toCardDef)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (capturedUnder, discardCaptured)

newtype StopTheExecutioners = StopTheExecutioners QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Capture 5. Time 4, as its card def says.
stopTheExecutioners :: QuestCard StopTheExecutioners
stopTheExecutioners = quest StopTheExecutioners Cards.stopTheExecutioners

-- | Forced: When the last time counter is removed from this quest, discard its captured
-- cards and remove this quest from the game. Then, raise each player's threat by 3 for
-- each of his characters that was discarded by this effect.
instance HasAbilities StopTheExecutioners where
  getAbilities (StopTheExecutioners a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

-- | Defeated, it leaves play for the victory display and its captured cards are rescued.
instance RunMessage StopTheExecutioners where
  runMessage msg q@(StopTheExecutioners attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      held <- capturedUnder attrs.cardId
      discardCaptured attrs.cardId
      removeFromGame attrs
      eachPlayer \pid -> do
        let characters = [c | c <- held, c.owner == Just pid, cdCardType (toCardDef c) `elem` [HeroType, AllyType]]
        encounterRaisesThreat pid (3 * length characters)
      pure q
    _ -> StopTheExecutioners <$> runMessage msg attrs
