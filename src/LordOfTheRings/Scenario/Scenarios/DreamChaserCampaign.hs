{- | The Dream-chaser Campaign Expansion's campaign cards (see "LordOfTheRings.Campaign"):
each scenario's setup, its Forced effects, and the resolution that adds boons and burdens
to the Campaign Pool and gains XP. A scenario runs them by falling through to
'runDreamChaserCampaign' in campaign mode; the setup is its ninth step, once the quest's
own setup has prepared the fleet and the Corsair deck.

What a campaign card's setup or Forced effect earns mid-game goes into the log the
scenario holds at once (the log is kept only once the scenario is won, as its resolution
would add it then), through these 'ScenarioSpecific' messages:

* @campaign.pool@: a boon or burden joins the Campaign Pool;
* @campaign.chosen@: a boon chosen during setup that adds 1 to Ominous Fate's X;
* @campaign.resource@: a resource on the campaign card, as Revenant places one.
-}
module LordOfTheRings.Scenario.Scenarios.DreamChaserCampaign (runDreamChaserCampaign, ominousFateBonus) where

import Data.Aeson.Types (parseMaybe)
import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Ally.Types (allyCardId, allyTokens)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (enemyTokens)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Game.Base (allPlayers, gameEntities, gamePlayerOrder)
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Campaign (templeSecrets)
import LordOfTheRings.Keyword (Keyword (Uncharted))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Grid (Pos (..))
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted.UpTo
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (objectiveUnderneath)
import LordOfTheRings.Placement (Placement (AttachedTo, InPlayArea), placementController)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Trait (Trait (Boon, Burden, DolAmroth, Ship, Underwater))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Id (AllyId, PlayerId, ScenarioId (..))
import LordOfTheRings.Modifier (ModifierType (Blank))
import LordOfTheRings.Timing (Timing (After))
import LordOfTheRings.Token (Token (Resource), countTokens)
import LordOfTheRings.Window (Window (..), WindowType (..))

-- | Ominous Fate's X past the campaign card's part number: 1 for each boon chosen during
-- setup that says so.
ominousFateBonus :: CampaignLog -> Int
ominousFateBonus = length . recorded ChosenDuringSetup

runDreamChaserCampaign :: Message -> ScenarioAttrs -> QueueT Message GameT ScenarioAttrs
runDreamChaserCampaign msg attrs = case attrs.campaign of
  Just l | l.campaign == "dream-chaser" -> runCampaign l msg attrs
  _ -> runMessage msg attrs

withLog :: (CampaignLog -> CampaignLog) -> ScenarioAttrs -> ScenarioAttrs
withLog f = campaignL %~ fmap f

sid :: ScenarioAttrs -> Text
sid = unScenarioId . (.scenarioId)

runCampaign :: CampaignLog -> Message -> ScenarioAttrs -> QueueT Message GameT ScenarioAttrs
runCampaign l msg attrs = case msg of
  DoStep 9 Setup -> do
    graxarsReward l
    attrs' <- setup l attrs
    runMessage msg attrs'
  ScenarioSpecific "campaign.pool" (parseMaybe parseJSON -> Just p) -> pure $ withLog (addToPool p) attrs
  ScenarioSpecific "campaign.chosen" (parseMaybe parseJSON -> Just code) ->
    pure $ withLog (\l' -> record ChosenDuringSetup (recorded ChosenDuringSetup l' <> [code]) l') attrs
  ScenarioSpecific "campaign.resource" _ -> pure $ withLog (\l' -> l' {campaignLogCardResources = l'.cardResources + 1}) attrs
  ScenarioSpecific "campaign.forget" (parseMaybe parseJSON -> Just key) ->
    pure $ withLog (\l' -> l' {campaignLogNotes = Map.delete key l'.notes}) attrs
  ScenarioSpecific "campaign.calphon" _ -> do
    -- Raid on the Grey Havens: Spyglass and Black Key (boon) on Calphon
    for_ [Attachments.spyglass, Attachments.blackKey] \def -> attachToAlly l def (AllyWithCardCode $ toCardCode Objectives.calphon)
    pure attrs
  ScenarioSpecific "campaign.blackSword" _ -> do
    swords <- getsGame (filter ((== toCardCode Objectives.theBlackSword) . toCardCode) . toList . entitiesObjectives . gameEntities)
    for_ swords \o -> placeResources ScenarioSource (toTarget o) 3
    pure attrs
  ScenarioSpecific "campaign.sahirFlipped" _ | null (recorded SahirTurned l) -> do
    turned <- selectAny sahirAlly
    if turned
      then do
        sahirTurned l
        pure $ withLog (record SahirTurned [toCardCode Objectives.captainSahirObjective]) attrs
      else pure attrs
  -- Revenant: Forced: After Revenant is destroyed, ... Place 1 resource on the campaign card.
  CheckWindows ws -> do
    when (any ((== toCardCode Enemies.revenant) . toCardCode) [c | Window After (EnemyDestroyed c) <- ws])
      $ push (ScenarioSpecific "campaign.resource" Null)
    attrs' <- windows l ws attrs
    runMessage msg attrs'
  -- The Thing in the Depths: Forced: After Captain Sahír flips over, ...
  -- (an enemy turned over is gone from play as this is seen: Captain Sahír's ally side is
  -- looked for once it has)
  Flip (EnemyTarget _) | sid attrs == "the-thing-in-the-depths" -> do
    push (ScenarioSpecific "campaign.sahirFlipped" Null)
    runMessage msg attrs
  -- The City of Corsairs: Forced: When stage 2B is revealed, search the encounter deck and
  -- discard pile for each boon and non-Ship burden and shuffle them into the second
  -- encounter deck.
  AdvanceToQuest card | sid attrs == "the-city-of-corsairs", toCardCode card == toCardCode Quests.fateOfTheDreamChaser -> do
    let carried c = let ts = cdCardTraits (toCardDef c) in Boon `member` ts || (Burden `member` ts && Ship `notMember` ts)
        moved = filter carried (attrs.encounterDeck <> attrs.encounterDiscard)
    runMessage msg
      $ attrs
      & encounterDeckL %~ filter (not . carried)
      & encounterDiscardL %~ filter (not . carried)
      & setAsideL %~ (<> moved)
  -- Temple of the Deceived: Travel Action: Flip over a secret at your location. If it is a
  -- Recurring Nightmare, it engages you. If it is Starla, you gain control of her (ready).
  UseCardAbility pid ScenarioSource 1 _ | sid attrs == "temple-of-the-deceived" -> do
    here <- mapMaybe (.position) <$> (traverse getLocation =<< select IsActiveLocation)
    let flipped = [(pos, code) | (pos, code) <- templeSecrets attrs, pos `elem` here]
    for_ flipped \(_, code) ->
      if code == toCardCode Enemies.recurringNightmare
        then putEnemyIntoPlayEngaged pid =<< genEncounterCard Enemies.recurringNightmare
        else putAllyIntoPlayUnder pid =<< genPlayerCard Allies.starla (Just pid)
    let rows = map (posRow . fst) flipped
        flip' l' = record RevealedSecrets (recorded RevealedSecrets l' <> map snd flipped) $ record Secrets [if r `elem` rows then "" else c | (r, c) <- zip [0 ..] (recorded Secrets l')] l'
    pure $ withLog flip' attrs
  ResolveCampaign -> do
    l' <- resolve l attrs
    runMessage msg (attrs & campaignL ?~ l' {campaignLogCardResources = 0, campaignLogNotes = foldr Map.delete l'.notes [ChosenDuringSetup, SahirKeyed, SahirTurned, Secrets, RevealedSecrets]})
  _ -> runMessage msg attrs

-- * Setup

-- | The card's side to use: its Upgraded side once XP has upgraded it.
side :: CampaignLog -> CardDef -> CardDef
side l def = toCardDef $ upgradedCode l (toCardCode def)

setup :: CampaignLog -> ScenarioAttrs -> QueueT Message GameT ScenarioAttrs
setup l attrs = case sid attrs of
  -- Each player chooses a Modification boon for each of their ship-objectives and
  -- attaches it to that ship-objective. For each ship-objective, its controller may deal 2
  -- damage to it to add 1 resource to a hero's resource pool. Shuffle Ominous Fate and
  -- Thunderstruck into the encounter deck. Shuffle Crowley into the Corsair deck.
  "voyage-across-belegaer" -> do
    modifications l
    shipsForResources
    shipBurdens l attrs [Treacheries.ominousFate]
  -- Attach Spyglass to Calphon. Shuffle Revenant into the encounter deck. For each
  -- character in play, its controller may deal 1 damage to it to reduce their threat by 1.
  "the-fate-of-numenor" -> do
    attachToAlly l Attachments.spyglass (AllyWithCardCode $ toCardCode Objectives.calphon)
    damageForThreat
    shuffleIn [Enemies.revenant] attrs
  -- The first player takes control of Calphon and attaches Spyglass and Black Key (boon)
  -- to him. Shuffle Graxar into the encounter deck. Each player may choose one copy of
  -- Galdor's Counsel and add it to their hand. For this game, X is plus 1 for each.
  "raid-on-the-grey-havens" -> do
    lead <- getLead
    putObjectiveAllyIntoPlayUnder lead =<< genEncounterCard Objectives.calphon
    push $ ScenarioSpecific "campaign.calphon" Null
    players <- getPlayerOrder
    eachChoosesOne "dreamChaserCampaign.galdorsCounsel" True players =<< traverse (\d -> genPlayerCard d Nothing) [Events.galdorsCounselLeadership, Events.galdorsCounselTactics, Events.galdorsCounselSpirit, Events.galdorsCounselLore]
    shuffleIn [Enemies.graxar] attrs
  -- Flip the Stormcaller to its Campaign Mode only side, then attach Black Key (burden) to
  -- it. Modifications, Thunderstruck and Crowley as Voyage Across Belegaer's. Put Calaerion
  -- into play under the control of the player with the Dream-chaser.
  "flight-of-the-stormcaller" -> do
    burdenOn l (EnemyWithCardCode $ toCardCode Enemies.stormcaller)
    modifications l
    calaerionJoins l
    shipBurdens l attrs []
  "the-thing-in-the-depths" -> pure attrs
  -- Attach Black Key (boon) to Captain Sahír. Shuffle Revenant into the encounter deck.
  -- Shuffle Starla and both Recurring Nightmares together and place one facedown under
  -- each Temple of the Deceived location, as secrets. For each character in play, its
  -- controller may deal 1 damage to it to reduce their threat by 1. One player may raise
  -- their threat by 4 to attach Spyglass to a hero.
  "temple-of-the-deceived" -> do
    attachToAlly l Attachments.blackKey sahirAlly
    secrets <- shuffleM [toCardCode Allies.starla, toCardCode Enemies.recurringNightmare, toCardCode Enemies.recurringNightmare]
    damageForThreat
    spyglassForThreat l
    shuffleIn [Enemies.revenant] attrs <&> withLog (record Secrets secrets)
  -- Attach Black Key (boon) to Captain Sahír. Shuffle Revenant into the encounter deck.
  -- Each player may choose one copy of Command and add it to their hand. For this game, X
  -- is plus 1 for each. One player may choose one copy of Círdan's Blessing and add it to
  -- their hand. For each character in play, its controller may deal 1 damage to it to
  -- reduce their threat by 1.
  "the-drowned-ruins" -> do
    attachToAlly l Attachments.blackKey sahirAlly
    players <- getPlayerOrder
    eachChoosesOne "dreamChaserCampaign.command" True players =<< traverse (\d -> genPlayerCard d Nothing) [Events.commandLeadership, Events.commandLore, Events.commandSpirit, Events.commandTactics]
    cirdansBlessing l
    damageForThreat
    shuffleIn [Enemies.revenant] attrs
  -- Attach Black Key (burden) to Raider Flagship. Modifications, Thunderstruck and
  -- Crowley. Put Calaerion into play under the player with the Dream-chaser. Put Gavin into
  -- play under any player's control. One player may choose a copy of Círdan's Blessing not
  -- in the Campaign Pool and add it to their hand. (Each Send for Help is set aside.)
  "a-storm-on-cobas-haven" -> do
    burdenOn l (EnemyWithCardCode $ toCardCode Enemies.raiderFlagship)
    modifications l
    calaerionJoins l
    gavinJoins
    cirdansBlessing l
    setAsideSendsForHelp =<< shipBurdens l attrs []
  -- As A Storm on Cobas Haven's, Black Key (burden) on the Stormcaller, Gavin only if
  -- "Gavin returned." Flip this card over.
  "the-city-of-corsairs" -> do
    burdenOn l (EnemyWithCardCode $ toCardCode Enemies.stormcallerTheCityOfCorsairs)
    modifications l
    calaerionJoins l
    when (isUpgraded "DCC210" l) gavinJoins
    cirdansBlessing l
    attrs' <- setAsideSendsForHelp =<< shipBurdens l attrs []
    pure $ attrs' & campaignCardFlippedL .~ True
  _ -> pure attrs

sahirAlly :: AllyMatcher
sahirAlly = AllyWithCardCode (toCardCode Objectives.captainSahirObjective)

-- | Thunderstruck into the encounter deck, Crowley into the Corsair deck, with the cards
-- given.
shipBurdens :: CampaignLog -> ScenarioAttrs -> [CardDef] -> QueueT Message GameT ScenarioAttrs
shipBurdens _ attrs others = do
  crowley <- genEncounterCard Enemies.crowley
  corsairs <- traverse (shuffleM . (crowley :)) attrs.corsairDeck
  shuffleIn (Enemies.thunderstruck : others) (attrs & corsairDeckL .~ corsairs)

shuffleIn :: [CardDef] -> ScenarioAttrs -> QueueT Message GameT ScenarioAttrs
shuffleIn defs attrs = do
  cards <- traverse genEncounterCard defs
  deck <- shuffleM (cards <> attrs.encounterDeck)
  pure $ attrs & encounterDeckL .~ deck

setAsideSendsForHelp :: ScenarioAttrs -> QueueT Message GameT ScenarioAttrs
setAsideSendsForHelp attrs = do
  cards <- traverse genEncounterCard [Treacheries.sendForHelpLeadership, Treacheries.sendForHelpTactics, Treacheries.sendForHelpSpirit, Treacheries.sendForHelpLore]
  pure $ attrs & setAsideL %~ (<> cards)

-- | The ship-objectives each player controls, in turn order.
fleetShips :: HasGame m => m [(PlayerId, AllyId)]
fleetShips = do
  players <- getPlayerOrder
  concat <$> for players \pid -> map (pid,) <$> select (AllyControlledBy (PlayerWithId pid) <> AllyWithTrait Ship)

modifications :: ReverseQueue m => CampaignLog -> m ()
modifications l = do
  ships <- fleetShips
  mods <- traverse (\d -> genPlayerCard (side l d) Nothing) [Attachments.armorPlating, Attachments.ballista, Attachments.enlargedQuarters, Attachments.extraSails]
  assign ships mods
 where
  assign :: ReverseQueue n => [(PlayerId, AllyId)] -> [PlayerCard] -> n ()
  assign [] _ = pure ()
  assign _ [] = pure ()
  assign ((pid, ship) : rest) mods = chooseOneM pid do
    questionLabeled "dreamChaserCampaign.modification"
    aimedAt [ship]
    showing (map PlayerCard mods)
    for_ mods \m -> targeting (CardIdTarget m.id) do
      attachOutOfPlay pid m ship
      assign rest (filter ((/= m.id) . (.id)) mods)

shipsForResources :: ReverseQueue m => m ()
shipsForResources = do
  ships <- fleetShips
  for_ ships \(pid, ship) -> do
    heroes <- select (HeroControlledBy $ PlayerWithId pid)
    chooseOneM pid do
      questionLabeled "dreamChaserCampaign.shipForResource"
      aimedAt [ship]
      labeled "dreamChaserCampaign.dealDamage" do
        dealDamage ScenarioSource ship 2
        chooseOrRunOneM pid do
          questionLabeled "chooseHero"
          targets heroes \h -> placeResources ScenarioSource h 1
      labeled "dreamChaserCampaign.decline" $ pure ()

damageForThreat :: ReverseQueue m => m ()
damageForThreat = eachPlayer \pid -> do
  characters <- selectCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
  chooseUpToM pid "dreamChaserCampaign.damageForThreat" ScenarioTarget [] (length characters)
    [UpToTarget c (dealDamage ScenarioSource c 1 >> lowerThreat pid 1) | c <- characters]

-- | Each player in turn may choose one of the cards left and add it to their hand, as
-- Galdor's Counsel or Command: it joins the Campaign Pool under their seat, and adds 1 to
-- Ominous Fate's X for this game when it says so.
eachChoosesOne :: ReverseQueue m => Text -> Bool -> [PlayerId] -> [PlayerCard] -> m ()
eachChoosesOne _ _ [] _ = pure ()
eachChoosesOne _ _ _ [] = pure ()
eachChoosesOne key raisesX (pid : rest) cards = do
  seat <- seatOf pid
  chooseOneM pid do
    questionLabeled key
    showing (map PlayerCard cards)
    for_ cards \c -> targeting (CardIdTarget c.id) do
      putInHand c {pcOwner = Just pid}
      push $ ScenarioSpecific "campaign.pool" (toJSON $ PoolCard (toCardCode c) seat Nothing)
      when raisesX $ push $ ScenarioSpecific "campaign.chosen" (toJSON $ toCardCode c)
      eachChoosesOne key raisesX rest (filter ((/= c.id) . (.id)) cards)
    labeled "dreamChaserCampaign.decline" $ eachChoosesOne key raisesX rest cards

seatOf :: HasGame m => PlayerId -> m (Maybe Int)
seatOf pid = getsGame (elemIndex pid . gamePlayerOrder)

-- | One player may choose a copy of Círdan's Blessing not in the Campaign Pool and add it
-- to their hand; it joins the Campaign Pool.
cirdansBlessing :: ReverseQueue m => CampaignLog -> m ()
cirdansBlessing l = do
  let pooled = map (.code) l.pool
  cards <- traverse (\d -> genPlayerCard d Nothing) [d | d <- [Attachments.cirdansBlessingLeadership, Attachments.cirdansBlessingTactics, Attachments.cirdansBlessingSpirit, Attachments.cirdansBlessingLore], toCardCode d `notElem` pooled]
  lead <- getLead
  unless (null cards) $ choosePlayerM lead ScenarioTarget \pid -> eachChoosesOne "dreamChaserCampaign.cirdansBlessing" False [pid] cards

-- | One player may raise their threat by 4 to attach Spyglass to a hero.
spyglassForThreat :: ReverseQueue m => CampaignLog -> m ()
spyglassForThreat l = do
  lead <- getLead
  heroes <- select AnyHero
  chooseOneM lead do
    questionLabeled "dreamChaserCampaign.spyglass"
    for_ heroes \h -> targeting h do
      owner <- (.owner) <$> getHero h
      raiseThreat owner 4
      card <- genPlayerCard (side l Attachments.spyglass) (Just owner)
      attachOutOfPlay owner card h
    labeled "dreamChaserCampaign.decline" $ pure ()

-- | A boon attachment on the ally, its controller's.
attachToAlly :: ReverseQueue m => CampaignLog -> CardDef -> AllyMatcher -> m ()
attachToAlly l def matcher = do
  allies <- traverse getAlly =<< select matcher
  for_ (take 1 allies) \a -> for_ (placementController a.placement) \pid -> do
    card <- genPlayerCard (side l def) (Just pid)
    attachOutOfPlay pid card a.id

-- | Black Key (burden) on the enemy.
burdenOn :: ReverseQueue m => CampaignLog -> EnemyMatcher -> m ()
burdenOn l matcher = do
  enemies <- select matcher
  for_ (take 1 enemies) \eid -> (`putObjectiveIntoPlayAttached` EnemyTarget eid) =<< genEncounterCard (side l Objectives.blackKeyBurden)

calaerionJoins :: ReverseQueue m => CampaignLog -> m ()
calaerionJoins l = do
  dreamChasers <- traverse getAlly =<< select (AllyWithTitle "Dream-chaser")
  lead <- getLead
  let pid = fromMaybe lead $ headMay (mapMaybe (placementController . (.placement)) dreamChasers)
  putObjectiveAllyIntoPlayUnder pid =<< genEncounterCard (side l Objectives.calaerion)

gavinJoins :: ReverseQueue m => m ()
gavinJoins = do
  lead <- getLead
  card <- genEncounterCard Objectives.gavin
  choosePlayerM lead ScenarioTarget \pid -> putObjectiveAllyIntoPlayUnder pid card

-- | Raid on the Grey Havens' resolution: If Graxar is in the victory display, choose either
-- Parrying Cutlass or Throwing Axe, add it to the Campaign Pool, and record which hero you
-- attach it to. Chosen as the next scenario is set up, its heroes in play.
graxarsReward :: ReverseQueue m => CampaignLog -> m ()
graxarsReward l = when (notNull $ recorded GraxarDefeated l) do
  push $ ScenarioSpecific "campaign.forget" (toJSON GraxarDefeated)
  lead <- getLead
  heroes <- select AnyHero
  chooseOneM lead do
    questionLabeled "dreamChaserCampaign.graxarsReward"
    for_ [Attachments.parryingCutlass, Attachments.throwingAxe] \def -> labeled ("dreamChaserCampaign." <> if def == Attachments.parryingCutlass then "parryingCutlass" else "throwingAxe") do
      chooseOneM lead do
        questionLabeled "dreamChaserCampaign.attachTo"
        targets heroes \h -> do
          chosen <- getHero h
          card <- genPlayerCard (side l def) (Just chosen.owner)
          attachOutOfPlay chosen.owner card h
          push $ ScenarioSpecific "campaign.pool" (toJSON $ PoolCard (toCardCode card) Nothing (Just $ toCardCode chosen))

-- * Forced effects

windows :: CampaignLog -> [Window] -> ScenarioAttrs -> QueueT Message GameT ScenarioAttrs
windows l ws attrs = case sid attrs of
  -- Raid on the Grey Havens: Forced: After stage 2B is revealed, remove Calphon and each
  -- attachment on him from the game, ignoring his "leaves play" ability.
  "raid-on-the-grey-havens" | Window After AdvancedToStage `elem` ws -> do
    stage <- getsGame (mapMaybe (cdStage . toCardDef) . toList . (.entities.quests))
    when (2 `elem` stage) do
      calphons <- select (AllyWithCardCode $ toCardCode Objectives.calphon)
      for_ calphons \c -> do
        calphon <- getAlly c
        phaseModifier ScenarioSource c Blank
        discard c
        removeFromEncounterDiscard (MkEncounterCard (allyCardId $ toAttrs calphon) (toCardCode calphon))
    pure attrs
  -- The Thing in the Depths: Forced: After Captain Sahír (enemy) enters play, attach Black
  -- Key (burden) to him.
  "the-thing-in-the-depths" -> keyed [Objectives.blackKeyBurden] (EnemyWithCardCode $ toCardCode Enemies.captainSahir)
  -- The City of Corsairs: Forced: When Captain Sahír enters play, attach Black Key
  -- (burden) and The Black Sword to him.
  "the-city-of-corsairs" -> keyed [Objectives.blackKeyBurden, Objectives.theBlackSword] (EnemyWithCardCode $ toCardCode Enemies.captainSahirTheCityOfCorsairs)
  _ -> pure attrs
 where
  -- once, the first time he is seen in play
  keyed defs sahir
    | notNull (recorded SahirKeyed l) = pure attrs
    | otherwise = do
        sahirs <- select sahir
        case sahirs of
          eid : _ -> do
            for_ defs \def -> (`putObjectiveIntoPlayAttached` EnemyTarget eid) =<< genEncounterCard (side l def)
            -- The Black Sword enters play with 3 resources on it.
            when (Objectives.theBlackSword `elem` defs) $ push (ScenarioSpecific "campaign.blackSword" Null)
            pure $ withLog (record SahirKeyed [toCardCode Enemies.captainSahir]) attrs
          [] -> pure attrs

-- | Captain Sahír turned over: remove Black Key (burden) from the game and attach Black
-- Key (boon) to him. Add Sea Serpent (Menacing side) to the staging area. One player adds
-- Come Together to their hand. For this game, X is plus 1 for Come Together.
sahirTurned :: ReverseQueue m => CampaignLog -> m ()
sahirTurned l = do
  attachToAlly l Attachments.blackKey sahirAlly
  addToStaging =<< genEncounterCard Enemies.seaSerpent
  lead <- getLead
  choosePlayerM lead ScenarioTarget \pid -> do
    seat <- seatOf pid
    putInHand =<< genPlayerCard Events.comeTogether (Just pid)
    push $ ScenarioSpecific "campaign.pool" (toJSON $ PoolCard (toCardCode Events.comeTogether) seat Nothing)
    push $ ScenarioSpecific "campaign.chosen" (toJSON $ toCardCode Events.comeTogether)

-- * Resolution

resolve :: CampaignLog -> ScenarioAttrs -> QueueT Message GameT CampaignLog
resolve l attrs = do
  players <- getsGame (toList . allPlayers)
  heroes <- select AnyHero
  heroDamage <- traverse (fmap (.damage) . getHero) heroes
  enemiesInPlay <- select AnyEnemy
  ships <- select shipEnemy
  stages <- getsGame (mapMaybe (cdStage . toCardDef) . filter (not . isSideQuestCard) . toList . (.entities.quests))
  let surviving = filter (not . (.eliminated)) players
      victory = attrs.victoryDisplay
      inVictory def = length $ filter ((== toCardCode def) . toCardCode) victory
      has def = inVictory def > 0
      xpIf b n = if b then n else 0
      onCourse = attrs.heading == Just Sunny
      threats40 = all ((<= 40) . (.threat)) surviving
      resources3 = min 3 l.cardResources
      common = length surviving - xpIf (has Treacheries.ominousFate) 1
      noDamage = all (== 0) heroDamage
  naasiyah <- resourcesOn (EnemyWithCardCode "11078") (AllyWithCardCode "11079")
  sahir <- resourcesOn (EnemyWithCardCode "11076") (AllyWithCardCode "11077")
  (xp, l') <- case sid attrs of
    "voyage-across-belegaer" -> do
      let stage2s = length [() | c <- victory, cdCardType (toCardDef c) == QuestType, cdStage (toCardDef c) == Just 2]
      pure
        ( common + xpIf (stage2s >= 4) 1 + xpIf onCourse 1 + xpIf (null ships) 1 + inVictory Enemies.corsairWarship
            + xpIf (has Enemies.crowley) 1 + xpIf (has Enemies.thunderstruck) 2
        , addToPool (PoolCard (toCardCode Treacheries.ominousFate) Nothing Nothing) l
        )
    "the-fate-of-numenor" ->
      pure (common + inVictory Locations.lushJungle + inVictory Locations.drownedGraves + resources3 + xpIf threats40 1, l)
    "raid-on-the-grey-havens" -> do
      burnt <- getsGame (concatMap (objectiveUnderneath . toAttrs) . filter ((== toCardCode Objectives.theHavensBurn) . toCardCode) . toList . entitiesObjectives . gameEntities)
      pure
        ( common + (if null burnt then 2 else xpIf (length burnt <= 2) 1) + inVictory Locations.elvenWaveRunner + xpIf (has Enemies.naasiyah) 1
            + xpIf (null enemiesInPlay) 1 + xpIf threats40 1
        , if has Enemies.graxar then record GraxarDefeated [toCardCode Enemies.graxar] l else l
        )
    "flight-of-the-stormcaller" ->
      pure
        ( common + xpIf onCourse 1 + xpIf (2 `elem` stages) 1 + xpIf (has Enemies.stormcaller) 2 + xpIf (has Enemies.crowley) 1
            + xpIf (has Enemies.thunderstruck) 2 + xpIf noDamage 1
        , l
        )
    "the-thing-in-the-depths" -> do
      enraged <- selectAny (EnemyWithCardCode $ toCardCode Enemies.seaSerpentEnraged)
      pure
        ( common + xpIf (sahir >= 5) 1 + xpIf (naasiyah >= 5) 1 + (if has Enemies.seaSerpentEnraged then 2 else xpIf enraged 1)
            + xpIf noDamage 1 + (if all ((<= 30) . (.threat)) surviving then 2 else xpIf threats40 1)
        , l
        )
    "temple-of-the-deceived" -> do
      starlas <- traverse getAlly =<< select (AllyWithCardCode $ toCardCode Allies.starla)
      starlaSeat <- traverse seatOf (mapMaybe (placementController . (.placement)) starlas)
      leadSeat <- seatOf =<< getLead
      halls <- select (LocationWithCardCode $ toCardCode Locations.templeHalls)
      faceupUncharted <- select (LocationWithKeyword Uncharted)
      let nightmares = length (filter (== toCardCode Enemies.recurringNightmare) (recorded RevealedSecrets l))
          earned =
            [PoolCard (toCardCode Allies.starla) seat Nothing | seat <- take 1 starlaSeat]
              <> replicate (max 0 nightmares) (PoolCard (toCardCode Enemies.recurringNightmare) leadSeat Nothing)
      pure
        ( common + resources3 + length halls + xpIf (length faceupUncharted <= 4) 1 + xpIf (has Enemies.templeGuardian) 1 + xpIf threats40 1
        , foldl' (flip addToPool) l earned
        )
    "the-drowned-ruins" -> do
      let underwater = length [() | c <- victory, Underwater `member` cdCardTraits (toCardDef c)]
      pure
        ( common + resources3 + xpIf (naasiyah >= 5) 1 + xpIf (null enemiesInPlay) 1 + xpIf (underwater >= 5) 1
            + xpIf (has Locations.shrineToMorgothTheDrownedRuins) 2 + xpIf threats40 1
        , l
        )
    "a-storm-on-cobas-haven" -> do
      warships <- select (AllyWithCardCode $ toCardCode Objectives.dolAmrothWarship)
      skirmishers <- select (AllyWithCardCode $ toCardCode Objectives.corsairSkirmisherShip)
      dolAmroth <- getsGame (filter (\o -> DolAmroth `member` cdCardTraits (toCardDef o) && claimed o.placement) . toList . entitiesObjectives . gameEntities)
      pure
        ( common + xpIf onCourse 1 + xpIf (length warships >= 2) 1 + xpIf (length skirmishers >= 2) 1 + xpIf (length dolAmroth >= 4) 1
            + inVictory Enemies.corsairWarship + xpIf (has Enemies.crowley) 1 + xpIf (has Enemies.raiderFlagship) 2 + xpIf (has Enemies.thunderstruck) 2
        , l
        )
    "the-city-of-corsairs" ->
      pure
        ( common + sum [xpIf (has d) 1 | d <- [Enemies.corsairWarship, Enemies.crowley, Enemies.stormcallerTheCityOfCorsairs]]
            + xpIf (has Locations.theShatteredMonument) 1 + xpIf (has Enemies.thunderstruck) 2
        , l
        )
    _ -> pure (0, l)
  pure $ gainXp xp l'
 where
  claimed = \case
    AttachedTo _ -> True
    InPlayArea _ -> True
    _ -> False
  isSideQuestCard q = cdCardType (toCardDef q) /= QuestType

-- | The resources on a double-sided card, whichever side is in play.
resourcesOn :: HasGame m => EnemyMatcher -> AllyMatcher -> m Int
resourcesOn enemyMatcher allyMatcher = do
  enemies <- traverse getEnemy =<< select enemyMatcher
  allies <- traverse getAlly =<< select allyMatcher
  pure $ sum (map (countTokens Resource . enemyTokens . toAttrs) enemies) + sum (map (countTokens Resource . allyTokens . toAttrs) allies)
