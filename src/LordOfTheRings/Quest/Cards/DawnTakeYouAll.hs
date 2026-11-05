module LordOfTheRings.Quest.Cards.DawnTakeYouAll (dawnTakeYouAll) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (trollEnemy)
import LordOfTheRings.Trait (Trait (Sack))

newtype DawnTakeYouAll = DawnTakeYouAll QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dawnTakeYouAll :: QuestCard DawnTakeYouAll
dawnTakeYouAll = quest DawnTakeYouAll Cards.dawnTakeYouAll

{- | When Revealed: Remove all Troll enemies and Sack cards from the game. If Troll Cave is
in the victory display, the players have discovered the treasure cards Sting, Glamdring,
and Orcrist.
The players have won the game.

In campaign mode every player has discovered them (see 'discoverTreasure').
-}
instance RunMessage DawnTakeYouAll where
  runMessage msg q@(DawnTakeYouAll attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      DawnTakeYouAll <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ removeFromGame =<< select trollEnemy
      traverse_ (removeFromGame . toId) =<< objectivesWithTrait Sack
      victory <- getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario))
      when (toCardCode Locations.trollCave `elem` victory) do
        traverse_ discoverTreasure [Attachments.stingTreasure, Attachments.glamdringTreasure, Attachments.orcristTreasure]
      winGame
      pure q
    _ -> DawnTakeYouAll <$> runMessage msg attrs
