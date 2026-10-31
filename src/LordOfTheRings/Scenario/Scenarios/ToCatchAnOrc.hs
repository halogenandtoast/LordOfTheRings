module LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc (toCatchAnOrc) where

import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (CardMatchAny), cardIs, cardMatches, getMainStage, getPlayer, getRemainingPlayers)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (getOutOfPlayDecks)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Window (..), WindowType (AdvancedToStage, DiscardedFromOutOfPlayDeck), mkAfter)

newtype ToCatchAnOrc = ToCatchAnOrc ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

toCatchAnOrc :: Scenario
toCatchAnOrc =
  Scenario
    $ scenario (ToCatchAnOrc . (nightmareDeckL ?~ nightmare)) "to-catch-an-orc" "To Catch an Orc"
 where
  nightmare =
    nightmareDeck
      Nightmare.toCatchAnOrcNightmare
      EncounterSet.ToCatchAnOrcNightmare
      [ (Enemies.prowlingWolf, 3)
      , (Enemies.orcSkirmisher, 1)
      , (Enemies.orcHunter, 1)
      , (Treacheries.orcHuntingParty, 2)
      , (Enemies.methedrasOrc, 1)
      , (Locations.methedras, 2)
      , (Locations.orcCave, 1)
      ]

-- | Set aside at setup: Mugash and his Guards, for Orders from Orthanc; in Nightmare mode,
-- the Vigilant Orcs and both Mugash's Rage too.
setAsideAtSetup :: CardMatcher
setAsideAtSetup =
  CardMatchAny
    [cardIs Enemies.mugashEnemy, cardIs Enemies.mugashsGuard, cardIs Enemies.vigilantOrc, cardIs Treacheries.mugashsRage]

{- | Mugash and Mugash's Guard have player card backs: they are set aside at once, for
Orders from Orthanc's setup to shuffle into the out-of-play decks. Each player's
out-of-play deck is kept here (see the Helpers).
-}
instance RunMessage ToCatchAnOrc where
  runMessage msg (ToCatchAnOrc attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.ToCatchAnOrc, EncounterSet.BrokenLands, EncounterSet.MistyMountainOrcs]
      let (mugash, encounterDeck) = partition (cardMatches setAsideAtSetup) cards
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
    --
    -- Nightmare: "Setup: Each player shuffles 1 copy of Vigilant Orc into his out of play
    -- deck. Set both copies of Mugash's Rage and any remaining copies of Vigilant Orc aside,
    -- out of play."
    ScenarioSpecific "outOfPlay.setup" _ -> do
      players <- getRemainingPlayers
      let (mugash, others) = partition (cardMatches $ cardIs Enemies.mugashEnemy) attrs.scenarioSetAside
          (guards, others') = partition (cardMatches $ cardIs Enemies.mugashsGuard) others
          (vigilant, stillAside) = partition (cardMatches $ cardIs Enemies.vigilantOrc) others'
          (shuffledIn, leftAside) = splitAt (length players - 1) guards
          vigilantFor = map pure vigilant <> repeat []
      dealt <- shuffleM (mugash <> shuffledIn)
      dealtDecks <- for (zip3 players dealt vigilantFor) \(pid, card, orc) -> do
        top <- take 20 . (.deck) <$> getPlayer pid
        for_ top \c -> removeFromDeck pid c.id
        (pid,) <$> shuffleM (map EncounterCard (card : orc) <> top)
      let vigilantLeft = drop (length players) vigilant
      pure $ ToCatchAnOrc $ withDecks (Map.fromList dealtDecks) & setAsideL .~ leftAside <> vigilantLeft <> stillAside
    -- Nightmare: "Forced: When the players advance to stage 3A, shuffle both copies of
    -- Mugash's Rage into the encounter deck."
    CheckWindows ws | attrs.nightmare, any ((== AdvancedToStage) . windowType) ws -> do
      stage <- getMainStage
      let rages = filter (cardMatches $ cardIs Treacheries.mugashsRage) attrs.scenarioSetAside
      when (stage == 3 && notNull rages) $ shuffleSetAsideIntoEncounterDeck rages
      ToCatchAnOrc <$> runMessage msg attrs
    ScenarioSpecific "outOfPlay.search" (parseMaybe parseJSON -> Just (pid, n)) -> do
      let (top, rest) = splitAt n (deckOf pid)
          playerCards = [c | c@(PlayerCard _) <- top]
      traverse_ addToStaging [ec | EncounterCard ec <- top]
      chooseOrRunOneM pid do
        questionLabeled "toCatchAnOrc.search"
        showing playerCards
        targets playerCards \chosen ->
          do
            for_ playerCards \case
              c@(PlayerCard pc) -> push $ if c == chosen then PutInHand pc else PutInDiscard pc
              EncounterCard _ -> pure ()
            discardedFrom pid (length playerCards > 1)
      pure $ ToCatchAnOrc $ withDecks $ Map.insert pid rest decks
    ScenarioSpecific "outOfPlay.reveal" (parseMaybe parseJSON -> Just pid) -> case deckOf pid of
      top : rest -> do
        case top of
          EncounterCard ec | cdCardType (toCardDef ec) == EnemyType -> addToStaging ec
          EncounterCard ec -> push (AddToEncounterDiscard [ec]) >> discardedFrom pid True
          PlayerCard pc -> push (PutInDiscard pc) >> discardedFrom pid True
        pure $ ToCatchAnOrc $ withDecks $ Map.insert pid rest decks
      [] -> pure $ ToCatchAnOrc attrs
    ScenarioSpecific "outOfPlay.discardTop" (parseMaybe parseJSON -> Just (pid, n)) -> do
      let (top, rest) = splitAt n (deckOf pid)
      for_ top \case
        EncounterCard ec | cdCardType (toCardDef ec) == EnemyType -> addToStaging ec
        EncounterCard ec -> push $ AddToEncounterDiscard [ec]
        PlayerCard pc -> push $ PutInDiscard pc
      discardedFrom pid (notNull top)
      pure $ ToCatchAnOrc $ withDecks $ Map.insert pid rest decks
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
    -- Guard Warg answers it
    discardedFrom :: ReverseQueue m => PlayerId -> Bool -> m ()
    discardedFrom pid any' = when any' $ push $ CheckWindows [mkAfter $ DiscardedFromOutOfPlayDeck pid]
