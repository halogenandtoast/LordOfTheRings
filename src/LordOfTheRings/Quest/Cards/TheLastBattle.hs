module LordOfTheRings.Quest.Cards.TheLastBattle (theLastBattle) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Keyword (Keyword (Battle, Siege))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheLastBattle = TheLastBattle QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLastBattle :: QuestCard TheLastBattle
theLastBattle = quest TheLastBattle Cards.theLastBattle

-- | Siege.
-- If the players have collected 4 or more victory points, The Last Battle gains battle and
-- loses siege.
instance HasModifiersFor TheLastBattle where
  getModifiersFor (TheLastBattle a) = do
    victory <- getsGame ((.victoryPoints) . toAttrs . (.scenario))
    when (victory >= 4) $ modified a a [GainKeyword Battle, LoseKeyword Siege]

-- | When Revealed: Reveal 1 card per player from the encounter deck and add it to the
-- staging area.
-- If the players defeat this stage, they have won the game.
instance RunMessage TheLastBattle where
  runMessage msg q@(TheLastBattle attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheLastBattle <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      revealEncounterCards . length =<< getRemainingPlayers
      pure q
    _ -> TheLastBattle <$> runMessage msg attrs
