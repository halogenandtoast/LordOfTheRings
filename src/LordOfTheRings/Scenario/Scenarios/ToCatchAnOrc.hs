module LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc (toCatchAnOrc) where

import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (CardMatcher (CardMatchAny), cardIs, cardMatches, getPlayer, getRemainingPlayers)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (getOutOfPlayDecks)

newtype ToCatchAnOrc = ToCatchAnOrc ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

toCatchAnOrc :: Scenario
toCatchAnOrc = Scenario $ scenario ToCatchAnOrc "to-catch-an-orc" "To Catch an Orc"

{- | Mugash and Mugash's Guard have player card backs: they are set aside at once, for
Orders from Orthanc's setup to shuffle into the out-of-play decks. Each player's
out-of-play deck is kept here (see the Helpers).
-}
instance RunMessage ToCatchAnOrc where
  runMessage msg (ToCatchAnOrc attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.ToCatchAnOrc, EncounterSet.BrokenLands, EncounterSet.MistyMountainOrcs]
      let (mugash, encounterDeck) = partition (cardMatches $ CardMatchAny [cardIs Enemies.mugashEnemy, cardIs Enemies.mugashsGuard]) cards
      firstStage <- genEncounterCard Quests.ordersFromOrthanc
      laterStages <- traverse genEncounterCard [Quests.searchingForMugash, Quests.theWizardsPrize]
      putQuestIntoPlay firstStage
      pure
        $ ToCatchAnOrc
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ mugash
    -- Orders from Orthanc: "Each player removes the top 20 cards of his deck and places
    -- them aside, as an out-of-play deck. The first player takes Mugash plus 1 Mugash's
    -- Guard for each other player in the game and randomly shuffles one facedown into each
    -- player's out-of-play deck. Place any remaining copies of Mugash's Guard aside, out of
    -- play."
    ScenarioSpecific "outOfPlay.setup" _ -> do
      players <- getRemainingPlayers
      let (mugash, guards) = partition (cardMatches $ cardIs Enemies.mugashEnemy) attrs.scenarioSetAside
          (shuffledIn, leftAside) = splitAt (length players - 1) guards
      dealt <- shuffleM (mugash <> shuffledIn)
      dealtDecks <- for (zip players dealt) \(pid, card) -> do
        top <- take 20 . (.deck) <$> getPlayer pid
        for_ top \c -> removeFromDeck pid c.id
        (pid,) <$> shuffleM (EncounterCard card : top)
      pure $ ToCatchAnOrc $ withDecks (Map.fromList dealtDecks) & setAsideL .~ leftAside
    ScenarioSpecific "outOfPlay.search" (parseMaybe parseJSON -> Just (pid, n)) -> do
      let (top, rest) = splitAt n (deckOf pid)
          playerCards = [c | c@(PlayerCard _) <- top]
      traverse_ addToStaging [ec | EncounterCard ec <- top]
      chooseOrRunOneM pid do
        questionLabeled "toCatchAnOrc.search"
        showing playerCards
        targets playerCards \chosen ->
          for_ playerCards \case
            c@(PlayerCard pc) -> push $ if c == chosen then PutInHand pc else PutInDiscard pc
            EncounterCard _ -> pure ()
      pure $ ToCatchAnOrc $ withDecks $ Map.insert pid rest decks
    ScenarioSpecific "outOfPlay.reveal" (parseMaybe parseJSON -> Just pid) -> case deckOf pid of
      top : rest -> do
        case top of
          EncounterCard ec | cdCardType (toCardDef ec) == EnemyType -> addToStaging ec
          EncounterCard ec -> push $ AddToEncounterDiscard [ec]
          PlayerCard pc -> push $ PutInDiscard pc
        pure $ ToCatchAnOrc $ withDecks $ Map.insert pid rest decks
      [] -> pure $ ToCatchAnOrc attrs
    ScenarioSpecific "outOfPlay.shuffleFromHand" (parseMaybe parseJSON -> Just pid) -> do
      hand <- (.hand) <$> getPlayer pid
      picked <- headMay <$> shuffleM hand
      case picked of
        Just card -> do
          push $ RemoveFromHand pid card.id
          deck <- shuffleM (card : deckOf pid)
          pure $ ToCatchAnOrc $ withDecks $ Map.insert pid deck decks
        Nothing -> pure $ ToCatchAnOrc attrs
    _ -> ToCatchAnOrc <$> runMessage msg attrs
   where
    decks = getOutOfPlayDecks attrs
    deckOf pid = Map.findWithDefault [] pid decks
    withDecks ds = attrs & metaL .~ toJSON ds
