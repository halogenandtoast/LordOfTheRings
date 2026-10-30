module LordOfTheRings.Quest.Cards.UnexpectedRescue (unexpectedRescue) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (chooseStartingHero, prepareCapturedDeck)

newtype UnexpectedRescue = UnexpectedRescue QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

unexpectedRescue :: QuestCard UnexpectedRescue
unexpectedRescue = quest UnexpectedRescue Cards.unexpectedRescue

-- | Setup: Each player shuffles his starting hand into his deck. Each player prepares a
-- separate captured deck (see insert) and chooses only 1 hero to be his starting hero. Each
-- player adds 2 resources to his starting hero and sets his threat to that hero's threat
-- cost. Set Southern Gate, Jailor Gornákh and the Angmar Orcs encounter set aside, out of
-- play. Shuffle the encounter deck. Each player draws a new starting hand of 3 cards.
-- When Revealed (1B): Each player creates his own staging area and advances to a different
-- stage 2A.
--
-- The scenario sets the encounter cards aside as it gathers them, and shuffles the deck.
instance RunMessage UnexpectedRescue where
  runMessage msg q@(UnexpectedRescue attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      eachPlayer \pid -> do
        prepareCapturedDeck pid
        heroes <- select (HeroControlledBy $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "unexpectedRescue.startingHero"
          causedBy attrs
          targets heroes (chooseStartingHero pid)
        drawCards attrs pid 3
      doStep 1 msg
      UnexpectedRescue <$> runMessage msg attrs
    DoStep 1 (Flip (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      questDeck <- getsGame ((.questDeck) . toAttrs . (.scenario))
      let (stages, rest) = partition ((== toCardCode Cards.gornakhsDungeons) . toCardCode) questDeck
      separatePlayers (map pure players)
      for_ (zip players stages) \(pid, stage) -> do
        focusAreaOf pid
        resetToStage stage rest
      for_ (take 1 players) focusAreaOf
      pure q
    _ -> UnexpectedRescue <$> runMessage msg attrs
