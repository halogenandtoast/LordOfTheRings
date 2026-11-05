module LordOfTheRings.Scenario.Scenarios.TheStewardsFear (theStewardsFear) where

import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Classes.HasQueue (peekQueue)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (entitiesLocations)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (locationCardId)
import LordOfTheRings.Objective.Types (objectiveCardId)
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Matcher
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers
import LordOfTheRings.Trait (Trait (Clue, Plot, Villain))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window

newtype TheStewardsFear = TheStewardsFear ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theStewardsFear :: Scenario
theStewardsFear =
  Scenario
    $ scenario (TheStewardsFear . (nightmareDeckL ?~ nightmare)) "the-stewards-fear" "The Steward's Fear"
 where
  -- the 2 new Plots join the 3 the hidden one is chosen from as Plots do
  nightmare =
    nightmareDeck
      Nightmare.theStewardsFearNightmare
      EncounterSet.TheStewardsFearNightmare
      [ (Enemies.pickpocket, 3)
      , (Locations.marketSquare, 2)
      , (Enemies.underworldDissident, 3)
      , (Locations.storehouse, 2)
      ]

{- | Setup (Conspiracy): Create the underworld deck from the enemies of Streets of Gondor and
Brigands and the 3 Clue objectives. Set Roots of Mindolluin aside, out of play. Shuffle
the Villain cards and set one aside at random, out of play, removing the others from the
game. Repeat this with the Plot cards.

Underworld X: when a location with it enters play, the top X cards of the underworld deck
are stacked facedown under it. When a location leaves play, the cards under it are
revealed one at a time and added to the staging area. A card from the underworld deck
that would be discarded goes to the encounter discard pile.
-}
instance RunMessage TheStewardsFear where
  runMessage msg (TheStewardsFear attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheStewardsFear, EncounterSet.Brigands, EncounterSet.StreetsOfGondor]
      -- Nightmare: "Setup: Shuffle each copy of False Accusations into the underworld deck."
      let fromHeirs c = cdEncounterSet (toCardDef c) `elem` map Just [EncounterSet.Brigands, EncounterSet.StreetsOfGondor]
          underworld c =
            (cdCardType (toCardDef c) == EnemyType && fromHeirs c)
              || cardMatches (CardWithTrait Clue) c
              || toCardCode c == toCardCode Treacheries.falseAccusations
          (underworldCards, rest) = partition underworld cards
          (villains, rest') = partition (cardMatches $ CardWithTrait Villain) rest
          (plots, rest'') = partition (cardMatches $ CardWithTrait Plot) rest'
          (roots, encounterDeck) = partition ((== toCardCode Locations.rootsOfMindolluin) . toCardCode) rest''
      deck <- shuffleM underworldCards
      villain <- take 1 <$> shuffleM villains
      (plot, removedPlots) <- splitAt 1 <$> shuffleM plots
      firstStage <- genEncounterCard Quests.conspiracy
      laterStages <- traverse genEncounterCard [Quests.theGrandDesign, Quests.theConfrontation]
      putQuestIntoPlay firstStage
      pure
        $ TheStewardsFear
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ villain <> plot <> roots
        & metaL .~ toJSON (Meta deck mempty removedPlots)
    CheckWindows ws -> do
      when attrs.nightmare $ nightmareWindows ws
      -- Underworld X, as a location enters the staging area; one being revealed with a
      -- When Revealed effect resolves that first, its Underworld after (ALeP 3.073, Sewers)
      queued <- lift peekQueue
      let revealing lid = any (\case CheckWindows ws' -> Window After (Revealed (LocationTarget lid)) `elem` ws'; _ -> False) queued
          waits lid = andM [pure (revealing lid), cdWhenRevealed . toCardDef <$> getLocation lid]
      entering <- filterM (fmap not . waits) [lid | Window After (EntersStagingArea (LocationTarget lid)) <- ws]
      revealed <-
        filterM
          (\lid -> andM [isJust <$> locationCard lid, cdWhenRevealed . toCardDef <$> getLocation lid])
          [lid | Window After (Revealed (LocationTarget lid)) <- ws]
      meta <- foldM stackUnder (getMeta attrs) (entering <> revealed)
      TheStewardsFear <$> runMessage msg (withMeta meta)
    -- Underworld X, as a location enters play as the active location, once it is in play
    PutIntoPlayAsActiveLocation _ -> do
      doStep 1 msg
      TheStewardsFear <$> runMessage msg attrs
    DoStep 1 (PutIntoPlayAsActiveLocation card) -> do
      lids <- getsGame (map toId . filter ((== card.id) . locationCardId . toAttrs) . toList . entitiesLocations . gameEntities)
      TheStewardsFear . withMeta <$> foldM stackUnder (getMeta attrs) lids
    ScenarioSpecific "underworld.place" (parseMaybe parseJSON -> Just (lid, n)) ->
      TheStewardsFear . withMeta <$> placeUnder lid n (getMeta attrs)
    ScenarioSpecific "underworld.reveal" _ -> do
      let meta = getMeta attrs
      case meta.underworldDeck of
        card : rest -> do
          if cardMatches (CardWithTrait Clue) card then push (AddToEncounterDiscard [card]) else revealOrStage card
          pure $ TheStewardsFear $ withMeta meta {underworldDeck = rest}
        [] -> pure $ TheStewardsFear attrs
    ScenarioSpecific "underworld.under" (parseMaybe parseJSON -> Just (lid, card)) ->
      locationCard lid >>= \case
        Just cid -> do
          placeUnderneath lid [EncounterCard card]
          let meta = getMeta attrs
          pure $ TheStewardsFear $ withMeta meta {underneath = Map.insertWith (flip (<>)) cid [card] meta.underneath}
        Nothing -> pure $ TheStewardsFear attrs
    ScenarioSpecific "underworld.revealUnder" (parseMaybe parseJSON -> Just lid) -> do
      let meta = getMeta attrs
      cid <- locationCard lid
      case cid >>= (`Map.lookup` meta.underneath) of
        Just cards -> do
          traverse_ revealOrStage cards
          pure $ TheStewardsFear $ withMeta meta {underneath = Map.delete (fromJustNote "card" cid) meta.underneath}
        Nothing -> pure $ TheStewardsFear attrs
    ScenarioSpecific "underworld.plot" _ -> do
      let meta = getMeta attrs
      case meta.removedPlots of
        [] -> pure $ TheStewardsFear attrs
        plots -> do
          plot <- uniform plots
          addToStaging plot
          pure $ TheStewardsFear $ withMeta meta {removedPlots = filter (/= plot) plots}
    -- the card, out of play or in the encounter discard pile, is shuffled into the underworld
    -- deck; one that should be in the discard pile and is not stays where it is
    ScenarioSpecific "underworld.reclaim" (parseMaybe parseJSON -> Just (card, fromDiscard))
      | not fromDiscard || card `elem` attrs.encounterDiscard || EncounterCard card `elem` attrs.victoryDisplay -> do
          let meta = getMeta attrs
          deck <- shuffleM (card : meta.underworldDeck)
          pure $ TheStewardsFear $ withMeta meta {underworldDeck = deck} & encounterDiscardL %~ filter (/= card) & victoryDisplayL %~ filter (/= EncounterCard card)
    ScenarioSpecific "underworld.shuffle" _ -> do
      let meta = getMeta attrs
      deck <- shuffleM (meta.underworldDeck <> attrs.encounterDeck)
      pure $ TheStewardsFear $ withMeta meta {underworldDeck = []} & encounterDeckL .~ deck
    Discard _ (LocationTarget lid) -> leaving lid
    AddToVictoryDisplay (LocationTarget lid) -> leaving lid
    ShuffleIntoEncounterDeck (LocationTarget lid) -> leaving lid
    ReturnToTopOfEncounterDeck (LocationTarget lid) -> leaving lid
    ReturnToBottomOfEncounterDeck (LocationTarget lid) -> leaving lid
    RemoveFromGame (LocationTarget lid) -> leaving lid
    _ -> TheStewardsFear <$> runMessage msg attrs
   where
    withMeta meta = attrs & metaL .~ toJSON meta
    -- a treachery from the underworld deck is revealed, its When Revealed resolving; the
    -- rest go to the staging area
    revealOrStage card
      | cdCardType (toCardDef card) == TreacheryType = push $ RevealCard card
      | otherwise = addToStaging card
    -- Nightmare: "Forced: After an enemy is destroyed, shuffle it into the underworld deck
    -- if you are not at stage 3." "Forced: At the beginning of the refresh phase, if there
    -- is a Clue card in the staging area, shuffle it back into the underworld deck."
    -- The enemy is taken from the encounter discard pile, or the victory display for one
    -- with victory points.
    nightmareWindows ws = do
      stage <- getMainStage
      when (stage /= 3) $ for_ [c | Window After (EnemyDestroyed (EncounterCard c)) <- ws] (reclaim True)
      when (Window After (PhaseBegins RefreshPhase) `elem` ws) do
        clues <- getsGame (filter ((== StagingArea) . (.placement)) . filter (cardMatches (CardWithTrait Clue)) . toList . (.entities.objectives))
        for_ clues \o -> do
          removeFromGame (ObjectiveTarget $ toId o)
          reclaim False (MkEncounterCard (objectiveCardId $ toAttrs o) (toCardCode o))
    reclaim fromDiscard card = push $ ScenarioSpecific "underworld.reclaim" (toJSON (card, fromDiscard :: Bool))
    locationCard lid = fmap (locationCardId . toAttrs) <$> getsGame (Map.lookup lid . entitiesLocations . gameEntities)
    stackUnder meta lid = do
      n <- underworldValue . toCardCode =<< getLocation lid
      if n > 0 then placeUnder lid n meta else pure meta
    -- the cards are stacked at once, as the keyword reads, before what answers the
    -- location entering play (ALeP 3.063); the message they are placed with tells the log,
    -- nothing else answering it
    placeUnder lid n meta =
      locationCard lid >>= \case
        Just cid -> do
          let (top, rest) = splitAt n meta.underworldDeck
          unless (null top) $ placeUnderneath lid (map EncounterCard top)
          pure meta {underworldDeck = rest, underneath = Map.insertWith (flip (<>)) cid top meta.underneath}
        Nothing -> pure meta
    -- the cards under the location are revealed one at a time into the staging area
    leaving lid = do
      let meta = getMeta attrs
      cid <- locationCard lid
      case cid >>= (`Map.lookup` meta.underneath) of
        Just cards -> do
          traverse_ revealOrStage cards
          TheStewardsFear <$> runMessage msg (withMeta meta {underneath = Map.delete (fromJustNote "card" cid) meta.underneath})
        Nothing -> TheStewardsFear <$> runMessage msg attrs
