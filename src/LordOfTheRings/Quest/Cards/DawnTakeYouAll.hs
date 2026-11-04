module LordOfTheRings.Quest.Cards.DawnTakeYouAll (dawnTakeYouAll) where

import LordOfTheRings.Campaign (CampaignLogUpdate (AddToPool), PoolCard (..))
import Data.List (elemIndex)
import LordOfTheRings.Card (CardCode, toCardCode)
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

{- | The treasure cards of the troll hoard: Sting, Orcrist and Glamdring (The Hobbit: Over
Hill and Under Hill, 20 to 22), by title and code.
-}
trollHoard :: [(Text, CardCode)]
trollHoard = [("Sting", "131020"), ("Glamdring", "131022"), ("Orcrist", "131021")]

{- | When Revealed: Remove all Troll enemies and Sack cards from the game. If Troll Cave is
in the victory display, the players have discovered the treasure cards Sting, Glamdring,
and Orcrist.
The players have won the game.

In campaign mode each treasure discovered goes to the deck of the player the first player
chooses for it, recorded in the Campaign Pool for that seat: a player may add only a
treasure he has discovered to his deck (rules insert, "Treasure").
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
      inCampaign <- getsGame (isJust . (.campaign) . toAttrs . (.scenario))
      when (toCardCode Locations.trollCave `elem` victory && inCampaign) do
        lead <- getLead
        players <- getRemainingPlayers
        order <- getsGame (.playerOrder)
        for_ trollHoard \(title, code) -> chooseOrRunOneM lead do
          questionLabeledWith "dawnTakeYouAllQuest.treasure" [textVar "treasure" title]
          causedBy attrs
          targets players \p ->
            updateCampaignLog $ AddToPool $ PoolCard code (elemIndex p order) Nothing
      winGame
      pure q
    _ -> DawnTakeYouAll <$> runMessage msg attrs
