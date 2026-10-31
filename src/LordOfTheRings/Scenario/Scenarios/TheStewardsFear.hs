module LordOfTheRings.Scenario.Scenarios.TheStewardsFear (theStewardsFear) where

import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Classes.HasQueue (peekQueue)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Entities (entitiesLocations)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (locationCardId)
import LordOfTheRings.Matcher
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers
import LordOfTheRings.Trait (Trait (Clue, Plot, Villain))
import LordOfTheRings.Window

newtype TheStewardsFear = TheStewardsFear ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theStewardsFear :: Scenario
theStewardsFear = Scenario $ scenario TheStewardsFear "the-stewards-fear" "The Steward's Fear"

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
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.TheStewardsFear, EncounterSet.Brigands, EncounterSet.StreetsOfGondor]
      let fromHeirs c = cdEncounterSet (toCardDef c) `elem` map Just [EncounterSet.Brigands, EncounterSet.StreetsOfGondor]
          underworld c = (cdCardType (toCardDef c) == EnemyType && fromHeirs c) || cardMatches (CardWithTrait Clue) c
          (underworldCards, rest) = partition underworld cards
          (villains, rest') = partition (cardMatches $ CardWithTrait Villain) rest
          (plots, rest'') = partition (cardMatches $ CardWithTrait Plot) rest'
          (roots, encounterDeck) = partition ((== toCardCode Locations.rootsOfMindolluin) . toCardCode) rest''
      deck <- shuffleM underworldCards
      villain <- take 1 <$> shuffleM villains
      plot <- take 1 <$> shuffleM plots
      firstStage <- genEncounterCard Quests.conspiracy
      laterStages <- traverse genEncounterCard [Quests.theGrandDesign, Quests.theConfrontation]
      putQuestIntoPlay firstStage
      pure
        $ TheStewardsFear
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ villain <> plot <> roots
        & metaL .~ toJSON (Meta deck mempty)
    CheckWindows ws -> do
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
          if cardMatches (CardWithTrait Clue) card then push (AddToEncounterDiscard [card]) else addToStaging card
          pure $ TheStewardsFear $ withMeta meta {underworldDeck = rest}
        [] -> pure $ TheStewardsFear attrs
    ScenarioSpecific "underworld.shuffle" _ -> do
      let meta = getMeta attrs
      deck <- shuffleM (meta.underworldDeck <> attrs.encounterDeck)
      pure $ TheStewardsFear $ withMeta meta {underworldDeck = []} & encounterDeckL .~ deck
    Discard (LocationTarget lid) -> leaving lid
    AddToVictoryDisplay (LocationTarget lid) -> leaving lid
    ShuffleIntoEncounterDeck (LocationTarget lid) -> leaving lid
    ReturnToTopOfEncounterDeck (LocationTarget lid) -> leaving lid
    ReturnToBottomOfEncounterDeck (LocationTarget lid) -> leaving lid
    RemoveFromGame (LocationTarget lid) -> leaving lid
    _ -> TheStewardsFear <$> runMessage msg attrs
   where
    withMeta meta = attrs & metaL .~ toJSON meta
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
          traverse_ addToStaging cards
          TheStewardsFear <$> runMessage msg (withMeta meta {underneath = Map.delete (fromJustNote "card" cid) meta.underneath})
        Nothing -> TheStewardsFear <$> runMessage msg attrs
