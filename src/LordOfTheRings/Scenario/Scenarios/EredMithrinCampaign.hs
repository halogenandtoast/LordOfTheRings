{- | The Ered Mithrin Campaign Expansion's campaign cards (MEC115; see
"LordOfTheRings.Campaign"): each scenario's setup, the text its campaign card keeps in play,
and the resolution that adds boons, burdens and encounter cards to the Campaign Pool. A
scenario runs them by falling through to 'runEredMithrinCampaign' in campaign mode, and
takes its abilities and modifiers from 'eredMithrinAbilities' and 'eredMithrinModifiers'.

The boons, burdens and treasures are named by card code: Turayn EMC166, A Growing Threat
EMC167, Melanwar EMC169, Isolde EMC171, The Threat to Wilderland EMC172, Dagnir's Spawn
EMC173, Abandoned Ruin EMC174, the Accolades EMC176-179, the Songs EMC180-183, Urdug's
Cunning EMC185, Misplaced Trust EMC186, Beldis EMC188, Ashen Brazier EMC190, Gundabad
Hunter EMC191, Gundabad Stalker EMC192, Dragon-sickness EMC194 and the treasures
EMC196-204 ("LordOfTheRings.Helpers.Treasure").

Treasure (MEC115 rules, "Treasure"): a treasure marked in the log may be added to a
player's deck before the game, no more than 1 copy by title; one is the group's once
discovered, whoever's heroes found it. "Look at 3 random undiscovered treasures" draws from
those neither in the log nor in this game already.
-}
module LordOfTheRings.Scenario.Scenarios.EredMithrinCampaign (
  runEredMithrinCampaign,
  eredMithrinAbilities,
  eredMithrinModifiers,
) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import LordOfTheRings.Ability (anyPlayer, mkAbility, response)
import LordOfTheRings.Matcher.Window (WindowMatcher (ExploredMatchingWindow))
import LordOfTheRings.Ability.Types (Ability, AbilityCost (Free), AbilityType (ActionAbility))
import LordOfTheRings.Ally.Types (allyTokens)
import LordOfTheRings.Enemy.Types (enemyTokens)
import LordOfTheRings.Attachment.Types (AttachmentAttrs (..))
import LordOfTheRings.Campaign
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (allPlayers, gameEntities)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Treasure (offerUndiscoveredTreasure)
import LordOfTheRings.Id (LocationId, PlayerId, ScenarioId (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (GuardRelease (AttachWhenFree), Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Types (QuestAttrs (questFlipped, questTokens), isSideQuest)
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (seatOf)
import LordOfTheRings.Scenario.Scenarios.TheWitheredHeath.Helpers (dragonSignsInVictoryDisplay)
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Timing (Timing (After))
import LordOfTheRings.Token (Token (Progress, Resource), countTokens)
import LordOfTheRings.Trait (Trait (Boon, Burden, Hazard))
import LordOfTheRings.Window (Window (..), WindowType (..))

isEredMithrin :: ScenarioAttrs -> Bool
isEredMithrin attrs = maybe False ((== "ered-mithrin") . (.campaign)) attrs.campaign

sid :: ScenarioAttrs -> Text
sid = unScenarioId . (.scenarioId)

withLog :: (CampaignLog -> CampaignLog) -> ScenarioAttrs -> ScenarioAttrs
withLog f = campaignL %~ fmap f

runEredMithrinCampaign :: Message -> ScenarioAttrs -> QueueT Message GameT ScenarioAttrs
runEredMithrinCampaign msg attrs = case attrs.campaign of
  Just l | l.campaign == "ered-mithrin" -> runCampaign l msg attrs
  _ -> runMessage msg attrs

-- * The cards

turayn, aGrowingThreat, melanwar, isolde, theThreatToWilderland, dagnirsSpawn, abandonedRuin :: CardCode
turayn = "EMC166"
aGrowingThreat = "EMC167"
melanwar = "EMC169"
isolde = "EMC171"
theThreatToWilderland = "EMC172"
dagnirsSpawn = "EMC173"
abandonedRuin = "EMC174"

-- | Master of the Hunt, Unbroken, The Steadfast and Dragon-slayer, for stages 1 to 4.
accolades :: [CardCode]
accolades = ["EMC176", "EMC177", "EMC178", "EMC179"]

-- | The Canny Huntsman, A Tale of Victories, Lay of Courage and Rhyme of the Slayer.
songs :: [CardCode]
songs = ["EMC180", "EMC181", "EMC182", "EMC183"]

urdugsCunning, misplacedTrust, beldis, ashenBrazier, gundabadHunter, gundabadStalker, dragonSickness :: CardCode
urdugsCunning = "EMC185"
misplacedTrust = "EMC186"
beldis = "EMC188"
ashenBrazier = "EMC190"
gundabadHunter = "EMC191"
gundabadStalker = "EMC192"
dragonSickness = "EMC194"

-- | Urdug, objective-ally side faceup (Roam Across Rhovanion).
urdug :: AllyMatcher
urdug = AllyWithCardCode (toCardCode Objectives.urdug)

runCampaign :: CampaignLog -> Message -> ScenarioAttrs -> QueueT Message GameT ScenarioAttrs
runCampaign l msg attrs = case msg of
  -- the Campaign Pool's encounter cards are shuffled in by the scenario runner; Abandoned
  -- Ruin's own Setup sends it to the Caves deck, if able, or out of the game; then each
  -- treasure in the log may go to a deck
  DoStep 2 Setup -> do
    attrs' <- runMessage msg attrs
    addTreasuresToDecks l
    let (ruins, rest) = partition ((== abandonedRuin) . toCardCode) attrs'.encounterDeck
    caves <- shuffleM (ruins <> attrs'.cavesDeck)
    pure $ if null ruins then attrs' else attrs' & encounterDeckL .~ rest & cavesDeckL %~ (\c -> if null c then c else caves)
  DoStep 9 Setup -> do
    attrs' <- setup l attrs
    push $ ScenarioSpecific "eredMithrin.threatToWilderland" Null
    runMessage msg attrs'
  ScenarioSpecific "eredMithrin.threatToWilderland" _ -> do
    threatToWilderlandResources l attrs
    pure attrs
  ResolveCampaign -> do
    resolve l attrs
    runMessage msg (attrs & campaignL ?~ l {campaignLogCardResources = 0, campaignLogNotes = foldr Map.delete l.notes [DefeatedStage, FramsShadeTreasure]})
  ScenarioSpecific "eredMithrin.discover" _ -> do
    controlled <- treasuresInPlay
    pure $ withLog (\l' -> foldl' (flip markTreasure) l' controlled) attrs
  ScenarioSpecific "eredMithrin.accoladesToPool" _ -> do
    attached <- accoladesInPlay
    for_ attached \(code, heroCode) -> updateCampaignLog $ AddToPool $ PoolCard code Nothing (Just heroCode)
    pure attrs
  ScenarioSpecific "eredMithrin.dragonSickness" _ -> do
    when (length (recorded Treasures l) >= 6) $ updateCampaignLog $ AddToPool $ PoolCard dragonSickness Nothing Nothing
    pure attrs
  ScenarioSpecific "eredMithrin.urdugsCunning" _ -> do
    let copies = length $ filter ((== urdugsCunning) . toCardCode) (attrs.encounterDeck <> attrs.encounterDiscard)
    if copies < 4
      then do
        card <- genEncounterCard (toCardDef urdugsCunning)
        deck <- shuffleM (card : attrs.encounterDeck)
        pure $ attrs & encounterDeckL .~ deck
      else pure attrs
  ScenarioSpecific "eredMithrin.framsShade" _ -> pure $ withLog (record FramsShadeTreasure [toCardCode Enemies.framsShade]) attrs
  -- The Withered Heath: the stage defeated, noted while its window is open
  CompleteQuest qid | sid attrs == "the-withered-heath" -> do
    quests <- getsGame (toList . entitiesQuests . gameEntities)
    let defeated = [toCardCode q | q <- quests, toId q == qid, not (isSideQuest q)]
    runMessage msg $ if null defeated then attrs else withLog (record DefeatedStage defeated) attrs
  CheckWindows ws -> do
    attrs' <- windows l ws attrs
    runMessage msg attrs'
  -- Roam Across Rhovanion: Forced: After the first player claims an objective, they may put
  -- their trust in Urdug by placing a resource on him (from the token pool). If they don't,
  -- shuffle a copy of the Urdug's Cunning burden into the encounter deck.
  ClaimObjective _ (HeroTarget hid) | sid attrs == "roam-across-rhovanion" -> do
    lead <- getLead
    claimer <- selectAny (HeroWithId hid <> HeroControlledBy (PlayerWithId lead))
    when claimer trustInUrdug
    runMessage msg attrs
  -- Journey Up the Anduin: Response: After you explore a non-unique location, put a
  -- resource here. Then, if there are X resources here, the players look at 3 random
  -- undiscovered treasures and choose 1 to attach to an eligible character. X is 3 more
  -- than the number of players in the game.
  UseCardAbility _ ScenarioSource 1 _ | sid attrs == "journey-up-the-anduin" -> do
    players <- length <$> getsGame allPlayers
    let n = l.cardResources + 1
    when (n == players + 3) $ offerTreasure attachTreasure
    pure $ withLog (\l' -> l' {campaignLogCardResources = n}) attrs
  -- The Ghost of Framsburg: Action: Raise each player's threat by 1 to shuffle the encounter
  -- discard pile into the encounter deck. (Any player may trigger this ability.)
  UseCardAbility _ ScenarioSource 2 _ | sid attrs == "the-ghost-of-framsburg" -> do
    eachPlayer (`raiseThreat` 1)
    shuffleEncounterDiscardIntoDeck AnyCard
    pure attrs
  _ -> runMessage msg attrs

-- | The campaign card's abilities in play.
eredMithrinAbilities :: ScenarioAttrs -> [Ability]
eredMithrinAbilities a
  | not (isEredMithrin a) = []
  | otherwise = case sid a of
      "journey-up-the-anduin" ->
        [anyPlayer $ mkAbility a 1 $ response $ ExploredMatchingWindow After (CardWithType LocationType <> NotCard UniqueCard)]
      "the-ghost-of-framsburg" -> [anyPlayer $ mkAbility a 2 $ ActionAbility Free]
      _ -> []

{- | What the campaign card keeps in play: the deep and discover keywords ignored where it
says so, Urdug's resources adding to his stats, and Mount Gundabad's unique locations kept
from encounter card effects that would discard them.
-}
eredMithrinModifiers :: HasModifiersM m => ScenarioAttrs -> m ()
eredMithrinModifiers a = when (isEredMithrin a) case sid a of
  "roam-across-rhovanion" -> do
    modified a GameTarget [IgnoreDeep]
    -- +1 willpower, threat, attack and defense for each resource: on the objective-ally
    -- side those it has, on the enemy side those it has
    urdugs <- select urdug
    for_ urdugs \u -> do
      n <- countTokens Resource . allyTokens . toAttrs <$> getAlly u
      when (n > 0) $ modified a u [WillpowerModifier n, AttackModifier n, DefenseModifier n]
    enemies <- select (EnemyWithCardCode $ toCardCode Enemies.urdugEnemy)
    for_ enemies \e -> do
      n <- countTokens Resource . enemyTokens . toAttrs <$> getEnemy e
      when (n > 0) $ modified a e [ThreatModifier n, AttackModifier n, DefenseModifier n]
  "fire-in-the-night" -> modified a GameTarget [IgnoreDeep]
  "the-ghost-of-framsburg" -> modified a GameTarget [IgnoreDeep]
  "mount-gundabad" -> do
    modified a GameTarget [IgnoreDiscover]
    uniques <- select UniqueLocation
    for_ uniques \lid -> modified a lid [CannotBeDiscardedByEncounterCards]
  "the-fate-of-wilderland" -> modified a GameTarget [IgnoreDeep, IgnoreDiscover]
  _ -> pure ()

-- * Setup

setup :: CampaignLog -> ScenarioAttrs -> QueueT Message GameT ScenarioAttrs
setup l attrs = case sid attrs of
  -- Any player gains control of Turayn. Shuffle A Growing Threat into the encounter deck.
  "journey-up-the-anduin" -> do
    anyPlayerGainsControl turayn
    shuffleIn [aGrowingThreat] attrs
  -- Any player gains control of Melanwar. The first player may search the encounter deck
  -- for a copy of Twilight Hall and add it to the staging area. If they do, the players look
  -- at 3 random undiscovered treasures and attach 1 of them to Twilight Hall as a guarded
  -- objective. When free of encounters, the players claim the treasure and attach it to any
  -- character.
  "lost-in-mirkwood" -> do
    anyPlayerGainsControl melanwar
    for_ (find ((== toCardCode Locations.twilightHall) . toCardCode) attrs.encounterDeck) \hall -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "eredMithrin.twilightHall"
        showing [EncounterCard hall]
        labeled "eredMithrin.searchTwilightHall" do
          removeFromEncounterDeck hall
          addToStaging hall
          shuffleEncounterDeck
          offerTreasure (guardPlayerCard hall.id AttachWhenFree)
        labeled "pass" $ pure ()
    pure attrs
  -- Each player may change hero cards without incurring the +1 threat penalty, and Haldan
  -- may be included among a player's heroes (see 'freeHeroChange' and 'lockedHeroes'). Any
  -- player gains control of Isolde. Add The Threat to Wilderland to the staging area,
  -- shuffle Dagnir's Spawn into the encounter deck, and shuffle Abandoned Ruin into the
  -- Caves deck. If "Turayn has taken an oath of vigilance," search the encounter deck for 3
  -- treacheries and remove them from the game.
  "the-kings-quest" -> do
    anyPlayerGainsControl isolde
    addToStaging =<< genEncounterCard (toCardDef theThreatToWilderland)
    ruin <- genEncounterCard (toCardDef abandonedRuin)
    caves <- shuffleM (ruin : attrs.cavesDeck)
    when (noted TuraynsOath) $ removeChosen 3 (filter isTreachery attrs.encounterDeck) removeFromEncounterDeck
    shuffleIn [dagnirsSpawn] (attrs & cavesDeckL .~ caves)
  -- A player may search their deck for a unique boon ally and add it to their hand. If
  -- "Melanwar has given you guidance on traversing Wilderland," search the encounter deck
  -- for 2 non-unique locations and the Caves deck for 1 location and remove them from the
  -- game. If "Isolde is on the hunt," search the encounter deck for 3 non-unique enemies
  -- and remove them from the game. Flip this card over.
  "the-withered-heath" -> do
    searchForBoonAlly
    when (noted MelanwarsGuidance) do
      removeChosen 2 (filter nonUniqueLocation attrs.encounterDeck) removeFromEncounterDeck
      removeChosen 1 (filter isLocation attrs.cavesDeck) takeFromCavesDeck
    when (noted IsoldeOnTheHunt) $ removeChosen 3 (filter nonUniqueEnemy attrs.encounterDeck) removeFromEncounterDeck
    pure $ attrs & campaignCardFlippedL .~ True
  -- A player may search their deck for a unique boon ally and add it to their hand. If
  -- "Melanwar has given you guidance on traversing Wilderland," search the encounter deck
  -- for 3 non-unique enemies not named Grey Mountain Giant and remove them from the game.
  -- The first player may put their trust in Urdug by placing a resource on him (from the
  -- token pool). If they don't, shuffle a copy of the Urdug's Cunning burden into the
  -- encounter deck. Flip this card over.
  "roam-across-rhovanion" -> do
    searchForBoonAlly
    when (noted MelanwarsGuidance)
      $ removeChosen 3 (filter (\c -> nonUniqueEnemy c && cdName (toCardDef c) /= "Grey Mountain Giant") attrs.encounterDeck) removeFromEncounterDeck
    trustInUrdug
    pure $ attrs & campaignCardFlippedL .~ True
  -- The first player gains control of Beldis. If "Turayn has taken an oath of vigilance,"
  -- search the encounter deck for 3 non-unique locations and remove them from the game. If
  -- "Isolde is on the hunt," search the encounter deck for 3 non-unique enemies and remove
  -- them from the game. Ignore the deep keyword ('eredMithrinModifiers').
  "fire-in-the-night" -> do
    lead <- getLead
    putAllyIntoPlayUnder lead =<< genPlayerCard (toCardDef beldis) (Just lead)
    when (noted TuraynsOath) $ removeChosen 3 (filter nonUniqueLocation attrs.encounterDeck) removeFromEncounterDeck
    when (noted IsoldeOnTheHunt) $ removeChosen 3 (filter nonUniqueEnemy attrs.encounterDeck) removeFromEncounterDeck
    pure attrs
  -- Each player may change hero cards without incurring the +1 threat penalty. Shuffle
  -- Ashen Brazier, Gundabad Hunter, and Gundabad Stalker into the encounter deck. A player
  -- may search their deck for a unique boon ally and add it to their hand. If "Isolde is on
  -- the hunt," search the encounter deck for 3 treacheries and remove them from the game. If
  -- "Beldis's care has bolstered the heroes' spirits," search the encounter deck for 3
  -- non-unique locations and remove them from the game. Flip this card.
  "the-ghost-of-framsburg" -> do
    searchForBoonAlly
    when (noted IsoldeOnTheHunt) $ removeChosen 3 (filter isTreachery attrs.encounterDeck) removeFromEncounterDeck
    when (noted BeldisCare) $ removeChosen 3 (filter nonUniqueLocation attrs.encounterDeck) removeFromEncounterDeck
    shuffleIn [ashenBrazier, gundabadHunter, gundabadStalker] (attrs & campaignCardFlippedL .~ True)
  -- If there are 6 or more treasures recorded in the campaign log, shuffle Dragon-sickness
  -- into the encounter deck. A player may search their deck for a unique boon ally and add
  -- it to their hand. If "Melanwar has given you guidance on traversing Wilderland," search
  -- the encounter deck for 2 non-unique locations and the Caves deck for 1 location and
  -- remove them from the game. If "Beldis's care has bolstered the heroes' spirits," search
  -- the encounter deck for 3 treacheries and remove them from the game. Flip this card.
  "mount-gundabad" -> do
    searchForBoonAlly
    when (noted MelanwarsGuidance) do
      removeChosen 2 (filter nonUniqueLocation attrs.encounterDeck) removeFromEncounterDeck
      removeChosen 1 (filter isLocation attrs.cavesDeck) takeFromCavesDeck
    when (noted BeldisCare) $ removeChosen 3 (filter isTreachery attrs.encounterDeck) removeFromEncounterDeck
    shuffleIn [dragonSickness | length (recorded Treasures l) >= 6] (attrs & campaignCardFlippedL .~ True)
  -- Each player may search their deck for a boon or treasure card and add it to their
  -- hand. If "Turayn has taken an oath of vigilance," search the encounter deck for 3
  -- non-unique enemies and remove them from the game. If "Beldis's care has bolstered the
  -- heroes' spirits," search the encounter deck for 3 treacheries and remove them from the
  -- game. Ignore the deep and discover keywords ('eredMithrinModifiers').
  "the-fate-of-wilderland" -> do
    eachPlayer \p -> searchDeck p ScenarioSource maxBound (CardMatchAny [CardWithTrait Boon, CardWithType TreasureType]) False
    when (noted TuraynsOath) $ removeChosen 3 (filter nonUniqueEnemy attrs.encounterDeck) removeFromEncounterDeck
    when (noted BeldisCare) $ removeChosen 3 (filter isTreachery attrs.encounterDeck) removeFromEncounterDeck
    pure attrs
  _ -> pure attrs
 where
  noted key = Map.member key l.notes
  typed t c = cdCardType (toCardDef c) == t
  isUnique c = cdUnique (toCardDef c)
  isTreachery = typed TreacheryType
  isLocation = typed LocationType
  nonUniqueLocation c = isLocation c && not (isUnique c)
  nonUniqueEnemy c = typed EnemyType c && not (isUnique c)

shuffleIn :: [CardCode] -> ScenarioAttrs -> QueueT Message GameT ScenarioAttrs
shuffleIn codes attrs = do
  cards <- traverse (genEncounterCard . toCardDef) codes
  deck <- shuffleM (cards <> attrs.encounterDeck)
  pure $ attrs & encounterDeckL .~ deck

-- | "Any player gains control of" the boon ally: the first player chooses who.
anyPlayerGainsControl :: ReverseQueue m => CardCode -> m ()
anyPlayerGainsControl code = do
  lead <- getLead
  choosePlayerM lead ScenarioTarget \p -> putAllyIntoPlayUnder p =<< genPlayerCard (toCardDef code) (Just p)

-- | "A player may search their deck for a unique boon ally and add it to their hand."
searchForBoonAlly :: ReverseQueue m => m ()
searchForBoonAlly = do
  lead <- getLead
  players <- getRemainingPlayers
  chooseOneM lead do
    questionLabeled "eredMithrin.searchForBoonAlly"
    targets players \p -> searchDeck p ScenarioSource maxBound (CardWithType AllyType <> UniqueCard <> CardWithTrait Boon) False
    labeled "pass" $ pure ()

{- | Search for the cards and remove them from the game: the first player chooses which, as
many as there are, and with no more than that, all of them go.
-}
removeChosen :: ReverseQueue m => Int -> [EncounterCard] -> (forall n. ReverseQueue n => EncounterCard -> n ()) -> m ()
removeChosen n cards remove
  | n <= 0 = pure ()
  | length cards <= n = traverse_ remove cards
  | otherwise = do
      lead <- getLead
      chooseOneM lead do
        questionLabeledWith "eredMithrin.removeFromGame" [intVar "n" n]
        showing (map EncounterCard cards)
        for_ (zip [1 ..] cards) \(i, c) -> targeting (CardIdTarget c.id) do
          remove c
          removeChosen (n - 1) (drop i cards) remove

-- | The Threat to Wilderland's Setup: "Add The Threat to Wilderland to the staging area with
-- X resources on it. X is the campaign card's part number." Read once the setup has put it
-- there: from the Campaign Pool at setup step 4, by The King's Quest's own setup at step 9.
threatToWilderlandResources :: ReverseQueue m => CampaignLog -> ScenarioAttrs -> m ()
threatToWilderlandResources l attrs = do
  objectives <- getsGame (toList . (.entities.objectives))
  for_ [o | o <- objectives, toCardCode o == theThreatToWilderland] \o ->
    placeResources ScenarioSource (toTarget o) (partNumber l attrs.scenarioId)

-- | Roam Across Rhovanion: the first player may put their trust in Urdug; if they don't, a
-- copy of Urdug's Cunning is shuffled into the encounter deck.
trustInUrdug :: ReverseQueue m => m ()
trustInUrdug = do
  lead <- getLead
  urdugs <- select urdug
  chooseOneM lead do
    questionLabeled "eredMithrin.trustInUrdug"
    for_ (take 1 urdugs) \u -> labeled "eredMithrin.trustUrdug" $ placeResources ScenarioSource u 1
    labeled "eredMithrin.distrustUrdug" $ push $ ScenarioSpecific "eredMithrin.urdugsCunning" Null

-- * Treasure

-- | Each treasure in the log may be added to one player's deck, the first player choosing
-- whose: the box holds one copy of each.
addTreasuresToDecks :: ReverseQueue m => CampaignLog -> m ()
addTreasuresToDecks l = do
  lead <- getLead
  players <- getRemainingPlayers
  for_ (recorded Treasures l) \code -> do
    shown <- genPlayerCard (toCardDef code) Nothing
    chooseOneM lead do
      questionLabeled "eredMithrin.addTreasure"
      showing [PlayerCard shown]
      targets players \p -> shuffleIntoDeck p . PlayerCard =<< genPlayerCard (toCardDef code) (Just p)
      labeled "eredMithrin.leaveTreasureOut" $ pure ()

treasuresInPlay :: HasGame m => m [CardCode]
treasuresInPlay = getsGame (map toCardCode . filter ((== TreasureType) . cdCardType . toCardDef) . toList . (.entities.attachments))

{- | "Look at 3 random undiscovered treasures and choose 1": the first player chooses for
the group what is done with it.
-}
offerTreasure :: ReverseQueue m => (forall n. ReverseQueue n => PlayerCard -> n ()) -> m ()
offerTreasure use = do
  lead <- getLead
  offerUndiscoveredTreasure lead ScenarioTarget "eredMithrin.chooseTreasure" use

-- | The treasure is attached to an eligible character, as its own "Attach to" reads.
attachTreasure :: ReverseQueue m => PlayerCard -> m ()
attachTreasure card = do
  lead <- getLead
  attachFreedCard lead card

-- | "The players discover each treasure they control": read once what came before it has
-- resolved.
discoverControlled :: ReverseQueue m => m ()
discoverControlled = push $ ScenarioSpecific "eredMithrin.discover" Null

-- * Forced effects and responses

windows :: CampaignLog -> [Window] -> ScenarioAttrs -> QueueT Message GameT ScenarioAttrs
windows l ws attrs = case sid attrs of
  -- Forced: After a main quest stage is defeated, any player who does not control an
  -- Accolade boon may attach one of the following boons to one of their heroes, based on
  -- which quest stage was defeated.
  "the-withered-heath"
    | notNull [() | Window After (QuestDefeated _) <- ws]
    , code : _ <- recorded DefeatedStage l -> do
        for_ (atMay accolades . subtract 1 =<< cdStage (toCardDef code)) offerAccolade
        pure $ withLog (\l' -> l' {campaignLogNotes = Map.delete DefeatedStage l'.notes}) attrs
  -- Response: After Fram's Shade engages a player while the players are at stage 1B or 2B,
  -- look at 3 random undiscovered treasures and attach 1 of them to Fram's Shade as a
  -- guarded objective. When the treasure is free of encounters, the players claim it and
  -- attach it to an eligible character. (Group limit once per game.)
  "the-ghost-of-framsburg"
    | not (Map.member FramsShadeTreasure l.notes)
    , eid : _ <- [eid | Window After (EnemyEngaged eid _) <- ws] -> do
        shade <- selectAny (EnemyWithId eid <> EnemyWithCardCode (toCardCode Enemies.framsShade))
        quests <- getsGame (toList . entitiesQuests . gameEntities)
        let atStage = or [questFlipped (toAttrs q) && cdStage (toCardDef q) `elem` [Just 1, Just 2] | q <- quests, not (isSideQuest q)]
        when (shade && atStage) do
          e <- getEnemy eid
          lead <- getLead
          chooseOneM lead do
            questionLabeled "eredMithrin.framsShade"
            labeled "eredMithrin.guardTreasure" do
              push $ ScenarioSpecific "eredMithrin.framsShade" Null
              offerTreasure (guardPlayerCard (toCard e).id AttachWhenFree)
            labeled "pass" $ pure ()
        pure attrs
  _ -> pure attrs

accoladesInPlay :: HasGame m => m [(CardCode, CardCode)]
accoladesInPlay = do
  attachments <- getsGame (toList . (.entities.attachments))
  heroes <- getsGame (toList . (.entities.heroes))
  pure
    [ (toCardCode a, toCardCode h)
    | a <- attachments
    , toCardCode a `elem` accolades
    , AttachedTo (HeroTarget hid) <- [attachmentPlacement (toAttrs a)]
    , h <- heroes
    , toId h == hid
    ]

-- | Any player who does not control an Accolade boon may attach this one to one of their
-- heroes: the first player chooses, or none does.
offerAccolade :: ReverseQueue m => CardCode -> m ()
offerAccolade code = do
  attachments <- getsGame (toList . (.entities.attachments))
  let present = [toAttrs a | a <- attachments, toCardCode a `elem` accolades]
      holders = [fromMaybe a.owner (attachmentController a) | a <- present]
  unless (code `elem` map toCardCode present) do
    players <- filter (`notElem` holders) <$> getRemainingPlayers
    heroes <- concat <$> for players \p -> map (p,) <$> select (HeroControlledBy $ PlayerWithId p)
    lead <- getLead
    shown <- genPlayerCard (toCardDef code) Nothing
    unless (null heroes) $ chooseOneM lead do
      questionLabeled "eredMithrin.accolade"
      showing [PlayerCard shown]
      for_ heroes \(p, h) -> targeting h do
        card <- genPlayerCard (toCardDef code) (Just p)
        attachOutOfPlay p card h
      labeled "pass" $ pure ()

-- * Resolution

resolve :: CampaignLog -> ScenarioAttrs -> QueueT Message GameT ()
resolve l attrs = case sid attrs of
  -- If the heroes choose to accept his aid, add Turayn to the campaign pool. Otherwise,
  -- mark that "Turayn has taken an oath of vigilance". The players discover each treasure
  -- they control. Add A Growing Threat and each Evil Creatures enemy in play to the pool.
  "journey-up-the-anduin" -> do
    allyJoins turayn TuraynsOath
    discoverControlled
    updateCampaignLog $ AddToPool $ PoolCard aGrowingThreat Nothing Nothing
    enemies <- getsGame (toList . (.entities.enemies))
    for_ [toCardCode e | e <- enemies, cdEncounterSet (toCardDef e) == Just EncounterSet.Wilderlands] \code ->
      updateCampaignLog $ AddEncounterCardToPool code
  -- Melanwar, or "Melanwar has given you guidance on traversing Wilderland". Discover.
  -- Discard cards from the encounter deck until X cards from either the Spiders of
  -- Mirkwood or Dol Guldur Orcs encounter set have been discarded and add those cards to
  -- the campaign pool. X is equal to the number of players in the game.
  "lost-in-mirkwood" -> do
    allyJoins melanwar MelanwarsGuidance
    discoverControlled
    players <- length <$> getsGame allPlayers
    shuffledDiscard <- shuffleM attrs.encounterDiscard
    let fromSets c = cdEncounterSet (toCardDef c) `elem` map Just [EncounterSet.SpidersOfMirkwood, EncounterSet.DolGuldurOrcs]
    for_ (take players $ filter fromSets (attrs.encounterDeck <> shuffledDiscard)) \c ->
      updateCampaignLog $ AddEncounterCardToPool (toCardCode c)
  -- Isolde, or "Isolde is on the hunt". Discover. Add Abandoned Ruin to the campaign pool.
  -- The first player chooses either The Threat to Wilderland or Dagnir's Spawn and adds it
  -- to the campaign pool.
  "the-kings-quest" -> do
    allyJoins isolde IsoldeOnTheHunt
    discoverControlled
    updateCampaignLog $ AddToPool $ PoolCard abandonedRuin Nothing Nothing
    lead <- getLead
    choices <- traverse (genEncounterCard . toCardDef) [theThreatToWilderland, dagnirsSpawn]
    chooseOneM lead do
      questionLabeled "eredMithrin.threatOrSpawn"
      showing (map EncounterCard choices)
      for_ choices \c -> targeting (CardIdTarget c.id) $ updateCampaignLog $ AddToPool $ PoolCard (toCardCode c) Nothing Nothing
  -- Add each Accolade boon in play to the campaign pool. Each player may choose one of the
  -- Songs and add it to the campaign pool. If there are 4 Dragon Sign objectives in the
  -- victory display, the players also look at 3 random undiscovered treasures and choose 1
  -- to attach to an eligible character. Discover.
  -- The game is won as Cold-Drake is defeated, which defeats stage 4 (it cannot be defeated
  -- otherwise while Cold-Drake is in play): its Accolade, Dragon-slayer, is offered first.
  "the-withered-heath" -> do
    for_ (atMay accolades 3) offerAccolade
    push $ ScenarioSpecific "eredMithrin.accoladesToPool" Null
    eachPlayerMayTakeOneOf "eredMithrin.chooseSong" Nothing (map toCardDef songs) \p song -> do
      seat <- seatOf p
      updateCampaignLog $ AddToPool $ PoolCard (toCardCode song) seat Nothing
    signs <- dragonSignsInVictoryDisplay
    when (signs >= 4) $ offerTreasure attachTreasure
    discoverControlled
  -- If there are 3 or more resources on Urdug, each player adds a copy of Misplaced Trust
  -- to the campaign pool (and their deck). Otherwise, each player adds a copy of Urdug's
  -- Cunning to the campaign pool. Each player chooses a location in the staging area and
  -- adds a copy of it to the campaign pool. Players must choose different locations, if
  -- able.
  "roam-across-rhovanion" -> do
    resources <- sum <$> (traverse (fmap (countTokens Resource . allyTokens . toAttrs) . getAlly) =<< select urdug)
    players <- getRemainingPlayers
    for_ players \p -> do
      seat <- seatOf p
      updateCampaignLog $ AddToPool $ if resources >= 3 then PoolCard misplacedTrust seat Nothing else PoolCard urdugsCunning Nothing Nothing
    chooseLocations players =<< select LocationInStagingArea
  -- Beldis, or "Beldis's care has bolstered the heroes' spirits". If there is at least 5
  -- progress on Hrogar's Hill, the players look at 3 random undiscovered treasures and
  -- choose 1 to attach to an eligible character. Discover. Each player may remove a
  -- non-burden card from the campaign pool.
  "fire-in-the-night" -> do
    allyJoins beldis BeldisCare
    quests <- getsGame (toList . entitiesQuests . gameEntities)
    let progress = sum [countTokens Progress (questTokens $ toAttrs q) | q <- quests, toCardCode q == toCardCode Quests.hrogarsHill]
    when (progress >= 5) $ offerTreasure attachTreasure
    discoverControlled
    players <- getRemainingPlayers
    for_ players \p -> do
      let removable = List.nub [pc | pc <- l.pool, Burden `notMember` cdCardTraits (toCardDef pc.code)]
      shown <- traverse (\pc -> (pc,) <$> genCard pc.code) removable
      unless (null shown) $ chooseOneM p do
        questionLabeled "eredMithrin.removeFromPool"
        showing (map snd shown)
        for_ shown \(pc, c) -> targeting (CardIdTarget c.id) $ updateCampaignLog $ RemoveOneFromPool pc
        labeled "pass" $ pure ()
  -- Discover. Add either Gundabad Hunter or Gundabad Stalker to the campaign pool. Shuffle
  -- the encounter discard pile into the encounter deck and discard cards from the top until
  -- 2 Hazard cards are discarded. The players, as a group, must choose 1 of those cards and
  -- add it to the campaign pool.
  "the-ghost-of-framsburg" -> do
    discoverControlled
    lead <- getLead
    gundabad <- traverse (genEncounterCard . toCardDef) [gundabadHunter, gundabadStalker]
    chooseOneM lead do
      questionLabeled "eredMithrin.hunterOrStalker"
      showing (map EncounterCard gundabad)
      for_ gundabad \c -> targeting (CardIdTarget c.id) $ updateCampaignLog $ AddToPool $ PoolCard (toCardCode c) Nothing Nothing
    deck <- shuffleM (attrs.encounterDiscard <> attrs.encounterDeck)
    let hazards = take 2 $ filter ((Hazard `member`) . cdCardTraits . toCardDef) deck
    chooseOrRunOneM lead do
      questionLabeled "eredMithrin.hazardToPool"
      showing (map EncounterCard hazards)
      for_ hazards \c -> targeting (CardIdTarget c.id) $ updateCampaignLog $ AddEncounterCardToPool (toCardCode c)
  -- The players may, as a group, look at 3 random undiscovered treasures and choose 1 to
  -- attach to an eligible character. Discover. Remove A Growing Threat from the campaign
  -- pool. If there are 6 or more treasures recorded in the campaign log, add
  -- Dragon-sickness to the campaign pool.
  "mount-gundabad" -> do
    lead <- getLead
    chooseOneM lead do
      questionLabeled "eredMithrin.lookAtTreasures"
      labeled "eredMithrin.takeTreasure" $ offerTreasure attachTreasure
      labeled "pass" $ pure ()
    discoverControlled
    updateCampaignLog $ RemoveFromPool aGrowingThreat
    push $ ScenarioSpecific "eredMithrin.dragonSickness" Null
  -- The players win the campaign!
  _ -> pure ()
 where
  genCard code =
    let def = toCardDef code
     in if isPlayerCardDef def then PlayerCard <$> genPlayerCard def Nothing else EncounterCard <$> genEncounterCard def

{- | "If the heroes choose to accept the ally's aid, add it to the campaign pool (any player
may add it to their deck). Otherwise, mark in the campaign log" the note: the first player
chooses for the group, and whose deck.
-}
allyJoins :: ReverseQueue m => CardCode -> CampaignLogKey -> m ()
allyJoins code key = do
  lead <- getLead
  players <- getRemainingPlayers
  shown <- genPlayerCard (toCardDef code) Nothing
  chooseOneM lead do
    questionLabeled "eredMithrin.allyJoins"
    showing [PlayerCard shown]
    targets players \p -> do
      seat <- seatOf p
      updateCampaignLog $ AddToPool $ PoolCard code seat Nothing
    labeled "eredMithrin.allyDeclined" $ updateCampaignLog $ Record key [code]

-- | Each player in turn chooses a location in the staging area to add a copy of to the
-- campaign pool, one not chosen before while any is left.
chooseLocations :: ReverseQueue m => [PlayerId] -> [LocationId] -> m ()
chooseLocations players staged = go players []
 where
  go :: ReverseQueue n => [PlayerId] -> [LocationId] -> n ()
  go [] _ = pure ()
  go (p : ps) chosen = do
    let left = filter (`notElem` chosen) staged
    chooseOneM p do
      questionLabeled "eredMithrin.locationToPool"
      for_ (if null left then staged else left) \lid -> targeting lid do
        code <- toCardCode <$> getLocation lid
        updateCampaignLog $ AddEncounterCardToPool code
        go ps (lid : chosen)
