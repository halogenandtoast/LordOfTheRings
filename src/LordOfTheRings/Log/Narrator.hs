{- | The game log, derived from the message stream, as in Arkham's "Arkham.Log.Narrator".

Cards do not write the log. Every change to the game passes through a 'Message', so
one 'case' here, knowing what each message means, gives the whole log in one place a
reader can review, and a new card logs correctly without doing anything.

Three rules:

1. __Silence is the default.__ Most messages are plumbing (windows, asks, steps), and a
   message nothing here claims says nothing.
2. __The narrator runs before the message.__ It reads the game as the message finds it:
   a card about to leave play is still there to be named, a count is still the old one.
3. __Never break the game.__ It only reads, never pushes, and never assumes something
   named in a message is still in play.
-}
module LordOfTheRings.Log.Narrator (narrationFor) where

import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities
import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyShadowRevealed))
import LordOfTheRings.Game.Base
import LordOfTheRings.Heading
import LordOfTheRings.Helpers.Character
import LordOfTheRings.Helpers.Staging
import LordOfTheRings.Id
import LordOfTheRings.Log
import LordOfTheRings.Log.Block
import LordOfTheRings.Log.Refs
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource)
import LordOfTheRings.Matcher (getArcheryTotal, getDamage, getDiscoverValue, getTimeCounters)
import LordOfTheRings.Modifier (ModifierType (CannotRemoveTimeCounters))
import LordOfTheRings.Message
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Placement
import LordOfTheRings.Player.Types
import LordOfTheRings.Prelude hiding (say)
import LordOfTheRings.Scenario.Scenarios.DesertCrossing.Helpers (getTemperature)
import LordOfTheRings.Scenario.Types
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window (TimeOfDay (..), Window (..), WindowType (Became, FlippedTo))

{- | What a message adds to the log, if anything. The match is pure, so a message that
says nothing costs nothing; only a narrating one reads the game.
-}
narrationFor :: HasGame m => Message -> Maybe (m [LogEntry])
narrationFor = \case
  -- \* Structure: the headings a reader finds their way by
  Setup -> Just do
    s <- getsGame (toAttrs . gameScenario)
    one $ structure [ikeyPart "log.scenarioBegins" ["scenario" ~> logRef RefScenario s.scenarioName]]
  BeginRound -> Just do
    -- before the round is counted, so the next one
    n <- getsGame ((+ 1) . gameRound)
    one $ structure [ikeyPart "log.round" ["count" ~> n]]
  BeginPhase phase -> Just $ one $ structure [ikeyPart ("log.phase." <> phaseKey phase) []]
  GameOver outcome -> Just $ one $ case outcome of
    Won -> toned Good $ structure [ikeyPart "log.gameOver.won" []]
    Lost -> toned Bad $ structure [ikeyPart "log.gameOver.lost" []]
  Surrender -> Just $ one $ toned Bad $ structure [ikeyPart "log.gameOver.surrendered" []]
  -- \* Blocks: an event that spans several answers, as one entry
  OpenLogBlock block -> Just $ openBlock block
  CloseLogBlock bid -> Just $ closeBlock bid
  -- \* What the players do
  PutAllyIntoPlay pid cid -> Just do
    who <- playerRefFor pid
    card <- targetRefFor (CardIdTarget cid)
    pure [action [ikeyPart "log.putsIntoPlay" ["player" ~> who, "card" ~> c]] | Just c <- [card]]
  PutAllyIntoPlayUnder _ card -> Just $ one $ mechanic [ikeyPart "log.putIntoPlay" ["card" ~> PlayerCard card]]
  PutHeroIntoPlay _ card -> Just $ one $ mechanic [ikeyPart "log.putIntoPlay" ["card" ~> PlayerCard card]]
  PutAttachmentIntoPlay _ card -> Just $ one $ mechanic [ikeyPart "log.putIntoPlay" ["card" ~> PlayerCard card]]
  SwapHero _ card -> Just $ one $ mechanic [ikeyPart "log.putIntoPlay" ["card" ~> PlayerCard card]]
  CommitToQuest target -> Just $ whenRef target \c -> do
    (name, strength) <- questingStat
    willpower <- strength target
    one $ action [ikeyPart "log.commits" ["character" ~> c, "willpower" ~> stat name willpower]]
  TravelTo lid -> Just $ whenRef (toTarget lid) \l -> one $ action [ikeyPart "log.travels" ["location" ~> l]]
  EngageEnemy pid eid -> Just $ whenRef (toTarget eid) \e -> do
    who <- playerRefFor pid
    one $ action [ikeyPart "log.engages" ["enemy" ~> e, "player" ~> who]]
  DefendWith _ _ defender -> Just $ whenRef defender \d -> one $ action [ikeyPart "log.defends" ["defender" ~> d]]
  DeclareAsDefender _ _ defender -> Just $ whenRef defender \d -> one $ action [ikeyPart "log.defends" ["defender" ~> d]]
  DefendedByMany _ _ defenders@(_ : _ : _) -> Just do
    refs <- catMaybes <$> traverse targetRefFor defenders
    one $ action [ikeyPart "log.defendMany" ["defenders" ~> refs]]
  RedirectAttack eid target -> Just $ whenRef (toTarget eid) \e -> whenRef target \t ->
    one $ action [ikeyPart "log.attackRedirected" ["enemy" ~> e, "target" ~> t]]
  UndefendedAttack pid _ -> Just do
    who <- playerRefFor pid
    one $ toned Bad $ action [ikeyPart "log.undefended" ["player" ~> who]]
  Mulligan pid -> Just do
    who <- playerRefFor pid
    one $ action [ikeyPart "log.mulligan" ["player" ~> who]]
  -- \* Threat
  RaiseThreat pid n | n > 0 -> Just $ threatChange "log.threatRaised" pid n (+ n)
  LowerThreat pid n | n > 0 -> Just $ threatChange "log.threatLowered" pid n (max 0 . subtract n)
  ScenarioSpecific "tharbad.lowerElimination" v ->
    Just $ one $ toned Bad $ mechanic [ikeyPart "log.eliminationLevelLowered" ["count" ~> fromMaybe (10 :: Int) (parseMaybe parseJSON v)]]
  -- \* The heat tracker (Desert Crossing): read before it moves, so the new temperature is worked out
  ScenarioSpecific "desertCrossing.temperature" (parseMaybe parseJSON -> Just (n :: Int)) | n /= 0 -> Just do
    t <- getTemperature
    one $ toned (if n > 0 then Bad else Good) $ mechanic [ikeyPart (if n > 0 then "log.temperatureRises" else "log.temperatureFalls") ["count" ~> abs n, "temperature" ~> max 0 (t + n)]]
  -- \* Day and Night, as The Wastes of Eriador's Time objective flips
  CheckWindows [Window _ (Became time)] -> Just $ one $ mechanic [ikeyPart (case time of Daytime -> "log.becomesDay"; Nighttime -> "log.becomesNight") []]
  -- a card turned over to its other side, as Beneath the Sands' Search objective: read
  -- once it has, so the side named is the one now faceup
  CheckWindows [Window _ (FlippedTo t _)] -> Just $ whenRef t \c -> one $ mechanic [ikeyPart "log.flippedTo" ["card" ~> c]]
  -- \* The heading and the Sailing test (Dream-chaser rules)
  ShiftHeading _ shift -> Just do
    heading <- getsGame ((.heading) . toAttrs . gameScenario)
    pure
      [ toned (if shift == ShiftOnCourse then Good else Bad)
        $ mechanic [ikeyPart (if h' == h then "log.headingStays" else "log.headingShifts") ["heading" ~> headingPart h']]
      | Just h <- [heading]
      , let h' = shiftHeading shift h
      ]
  SailingTest -> Just $ one $ mechanic [ikeyPart "log.sailingTest" []]
  ResolveSailingTest committed -> Just do
    refs <- catMaybes <$> traverse targetRefFor committed
    one $ action [if null refs then ikeyPart "log.sailingTestNoCommits" [] else ikeyPart "log.sailingTestCommits" ["characters" ~> refs]]
  SailingTestResult top found canceled -> Just $ one $ toned (if found > canceled then Good else Bad) $ test
    $ ikeyPart "log.sailingTestResult" ["cards" ~> map EncounterCard top, "count" ~> found]
    : [ikeyPart "log.sailingIconCanceled" [] | canceled > 0]
  PrepareCorsairDeck -> Just $ one $ mechanic [ikeyPart "log.corsairDeckPrepared" []]
  ScenarioSpecific "cityOfCorsairs.switchEncounterDeck" _ -> Just $ one $ mechanic [ikeyPart "log.secondEncounterDeckActive" []]
  PutObjectiveAllyIntoPlayUnder _ card -> Just $ one $ mechanic [ikeyPart "log.putIntoPlay" ["card" ~> EncounterCard card]]
  EliminatePlayer pid -> Just do
    who <- playerRefFor pid
    one $ toned Bad $ mechanic [ikeyPart "log.eliminated" ["player" ~> who]]
  SeparatePlayers (_ : _ : _) -> Just $ one $ mechanic [ikeyPart "log.separated" []]
  -- an eliminated player's area leaves with him, joining no one
  JoinArea joining target -> Just do
    eliminated <- getsGame (maybe True (.eliminated) . Map.lookup joining . allPlayers)
    who <- playerRefFor joining
    other <- playerRefFor target
    pure [mechanic [ikeyPart "log.joins" ["player" ~> who, "other" ~> other]] | not eliminated]
  -- \* Damage and tokens
  DoStep 1 (DealDamage source target n) | n > 0 -> Just $ whenRef target \t -> do
    mSource <- sourceRefFor source
    one $ mechanic $ pure $ case mSource of
      Just s | s /= t -> ikeyPart "log.takesDamageFrom" ["target" ~> t, "count" ~> n, "source" ~> s]
      _ -> ikeyPart "log.takesDamage" ["target" ~> t, "count" ~> n]
  HealDamage _ target n | n > 0 -> Just $ whenRef target \t -> do
    healed <- min n <$> getDamage target
    pure [mechanic [ikeyPart "log.heals" ["target" ~> t, "count" ~> healed]] | healed > 0]
  PlaceTokens source target token n | n > 0 -> Just do
    phase <- getsGame gamePhase
    case token of
      Progress -> whenRef target \t -> one $ mechanic [ikeyPart "log.progress" ["target" ~> t, "progress" ~> stat "progress" n]]
      -- every hero's resource for the round says nothing the board does not
      Resource | source == GameSource && phase == ResourcePhase -> pure []
      Resource -> tokens "log.gainsResources"
      -- damage placed on a location, as Celebrimbor's Secret's encounter cards place it
      Damage | LocationTarget _ <- target -> tokens "log.damagePlaced"
      -- dealt with as damage, where the source is known
      Damage -> pure []
      Villager -> tokens "log.villagersPlaced"
      TimeCounter -> tokens "log.timeCountersPlaced"
   where
    tokens key = whenRef target \t -> one $ mechanic [ikeyPart key ["target" ~> t, "count" ~> n]]
  RemoveTokens source target token n | n > 0 -> case token of
    Progress -> Just $ whenRef target \t -> do
      -- a location immune to the source keeps its progress, as East-gate against Cave In
      immune <- case target of
        LocationTarget _ -> immuneToSource source target
        _ -> pure False
      mSource <- sourceRefFor source
      one $ mechanic $ pure $ case mSource of
        Just s | immune -> ikeyPart "log.immuneTo" ["target" ~> t, "source" ~> s]
        _ -> ikeyPart "log.progressRemoved" ["target" ~> t, "progress" ~> stat "progress" n]
    Resource -> Just $ whenRef target \t -> one $ notice [ikeyPart "log.spendsResources" ["target" ~> t, "count" ~> n]]
    -- damage taken off a location, as Sahír's Advance removes it in place of progress
    Damage | LocationTarget _ <- target -> Just $ whenRef target \t -> one $ toned Good $ mechanic [ikeyPart "log.damageRemoved" ["target" ~> t, "count" ~> n]]
    Damage -> Nothing
    Villager -> Just $ whenRef target \t -> one $ mechanic [ikeyPart "log.villagersRemoved" ["target" ~> t, "count" ~> n]]
    TimeCounter -> Just $ whenRef target \t -> do
      barred <- elem CannotRemoveTimeCounters <$> getModifiers target
      removed <- min n <$> getTimeCounters target
      pure [mechanic [ikeyPart "log.timeCountersRemoved" ["target" ~> t, "count" ~> removed]] | removed > 0, not barred]
  -- what a card does to characters; elsewhere readying and exhausting are routine
  Ready target -> Just $ duringCard $ simply "log.readies" target
  Exhaust target -> Just $ duringCard $ simply "log.exhausts" target
  -- \* Leaving play
  Destroy _ target -> Just $ whenRef target \t ->
    -- a character going down is bad; an enemy going down is the point
    one
      $ toned (case target of EnemyTarget _ -> Good; _ -> Bad)
      $ mechanic [ikeyPart "log.destroyed" ["target" ~> t]]
  Discard target -> Just $ whenRef target \t -> do
    -- a destroyed character is discarded as it goes, and was already said
    destroyed <- case target of
      HeroTarget _ -> (>=) <$> getDamage target <*> getHitPoints target
      AllyTarget _ -> (>=) <$> getDamage target <*> getHitPoints target
      _ -> pure False
    resolved <- case target of
      -- a treachery that has resolved is discarded as a matter of course
      TreacheryTarget tid ->
        maybe True ((== Unplaced) . (.placement))
          . Map.lookup tid
          <$> getsGame (entitiesTreacheries . gameEntities)
      _ -> pure False
    pure [mechanic [ikeyPart "log.discarded" ["target" ~> t]] | not (destroyed || resolved)]
  ReturnToHand target -> Just $ simply "log.returnsToHand" target
  ShuffleIntoOwnersDeck target -> Just $ simply "log.shuffledIntoOwnersDeck" target
  PutOnBottomOfOwnersDeck target -> Just $ simply "log.putOnBottomOfOwnersDeck" target
  ShuffleIntoEncounterDeck target -> Just $ simply "log.shuffledIntoEncounterDeck" target
  ReturnToTopOfEncounterDeck target -> Just $ simply "log.returnedToTopOfEncounterDeck" target
  RemoveFromQuest target -> Just $ simply "log.removedFromQuest" target
  -- an enemy or objective a card adds to the victory display, as a Capture objective takes a
  -- Wild Mûmak with it (a destroyed enemy goes there without this message)
  AddToVictoryDisplay target@(EnemyTarget _) -> Just $ simply "log.addedToVictoryDisplay" target
  AddToVictoryDisplay target@(ObjectiveTarget _) -> Just $ simply "log.addedToVictoryDisplay" target
  ReturnToBottomOfEncounterDeck target -> Just $ simply "log.returnedToBottomOfEncounterDeck" target
  -- \* The encounter deck
  AddToStaging card -> Just $ one $ mechanic [ikeyPart "log.addedToStaging" ["card" ~> EncounterCard card]]
  PutShadowEnemyIntoStaging eid cid -> Just do
    mCard <- getsGame (\g -> find ((== cid) . (.id)) . (.shadowCards) =<< Map.lookup eid g.entities.enemies)
    pure [mechanic [ikeyPart "log.addedToStaging" ["card" ~> EncounterCard c]] | Just c <- [mCard]]
  PutObjectiveIntoPlayAttached card host -> Just $ whenRef host \h ->
    one $ mechanic [ikeyPart "log.attachedTo" ["card" ~> EncounterCard card, "target" ~> h]]
  RemoveFromGame target -> Just $ simply "log.removedFromGame" target
  RemoveSetAsideFromGame cards | notNull cards -> Just $ one $ mechanic [ikeyPart "log.setAsideRemovedFromGame" ["cards" ~> map EncounterCard cards]]
  SetAsideHero hid -> Just $ simply "log.setAsideOutOfPlay" (HeroTarget hid)
  SetAsideEnemy eid -> Just $ simply "log.setAsideOutOfPlay" (EnemyTarget eid)
  PutIntoPlayAsActiveLocation card -> Just $ one $ mechanic [ikeyPart "log.becomesActive" ["location" ~> EncounterCard card]]
  DiscardTopOfEncounterDeck _ _ n | n > 0 -> Just do
    top <- getsGame (take n . (.encounterDeck) . toAttrs . gameScenario)
    pure [mechanic [ikeyPart "log.discardedFromEncounterDeck" ["cards" ~> map EncounterCard top]] | notNull top]
  TriggerLostEffects _ -> Just $ one $ toned Bad $ mechanic [ikeyPart "log.lostEffects" []]
  PutObjectiveIntoPlay card -> Just $ one $ mechanic [ikeyPart "log.placedInStaging" ["card" ~> EncounterCard card]]
  PutObjectiveIntoPlayGuardedBy card _ -> Just $ one $ mechanic [ikeyPart "log.placedInStaging" ["card" ~> EncounterCard card]]
  -- \* The Discover and Loot keywords (MEC69 rules)
  ResolveDiscover pid lid -> Just $ getDiscoverValue lid >>= \case
    Just x -> whenRef (toTarget lid) \l -> do
      who <- playerRefFor pid
      one $ mechanic [ikeyPart "log.resolvesDiscover" ["player" ~> who, "location" ~> l, "count" ~> x]]
    Nothing -> pure []
  DiscardLoot card -> Just $ one $ mechanic [ikeyPart "log.lootDiscarded" ["card" ~> EncounterCard card]]
  EngageOrcGuard pid _ -> Just do
    who <- playerRefFor pid
    one $ mechanic [ikeyPart "log.orcGuardEngages" ["player" ~> who]]
  -- the card goes face down, so only whose it was is said
  Reanimate cid _ -> Just do
    owner <- getsGame \g -> headMay [p.id | p <- toList g.players, c <- p.deck <> p.hand <> p.discard, c.id == cid]
    case owner of
      Nothing -> pure []
      Just pid -> do
        who <- playerRefFor pid
        one $ toned Bad $ mechanic [ikeyPart "log.reanimates" ["player" ~> who]]
  TakeHiddenCard pid -> Just do
    who <- playerRefFor pid
    one $ mechanic [ikeyPart "log.takesHiddenCard" ["player" ~> who]]
  TurnHiddenCardFaceup pid card _ -> Just $ whenHidden pid card \who ->
    one $ mechanic [ikeyPart "log.turnsHiddenCardFaceup" ["player" ~> who, "card" ~> EncounterCard card]]
  DiscardHiddenCard pid card -> Just $ whenHidden pid card \who ->
    one $ mechanic [ikeyPart "log.discardsHiddenCard" ["player" ~> who, "card" ~> EncounterCard card]]
  DrawTreacheryIntoHand pid tid -> Just $ whenRef (toTarget tid) \t -> do
    who <- playerRefFor pid
    one $ toned Bad $ mechanic [ikeyPart "log.drawsIntoHand" ["player" ~> who, "card" ~> t]]
  AttachShadowCard eid _ -> Just $ whenRef (toTarget eid) \e -> one $ mechanic [ikeyPart "log.dealtShadow" ["enemy" ~> e]]
  -- an encounter card in play set to guard an objective, as Heavy Cruiser attaches to one
  -- (one revealed by the Guarded keyword is not yet in play, its reveal told instead)
  SetObjectiveGuard oid cid -> Just do
    es <- getsGame gameEntities
    let guard' = headMay $ [toTarget e | e <- toList es.enemies, (toCard e).id == cid] <> [toTarget l | l <- toList es.locations, (toCard l).id == cid]
    flip (maybe (pure [])) guard' \t -> whenRef (ObjectiveTarget oid) \o -> whenRef t \g ->
      one $ mechanic [ikeyPart "log.guardsObjective" ["guard" ~> g, "objective" ~> o]]
  -- an enemy with two sides turns over, as Thaurdir does
  Flip (EnemyTarget eid) -> Just $ whenRef (toTarget eid) \e -> one $ mechanic [ikeyPart "log.enemyFlips" ["enemy" ~> e]]
  -- a hero turning to its enemy side, as Sméagol to Gollum
  Flip (HeroTarget hid) -> Just do
    hs <- getsGame (entitiesHeroes . gameEntities)
    case Map.lookup hid hs of
      Just h | Just other <- cdOtherSide (toCardDef h), cdCardType (toCardDef other) == EnemyType -> whenRef (toTarget hid) \from ->
        one $ toned Bad $ mechanic [ikeyPart "log.heroFlips" ["hero" ~> from, "card" ~> PlayerCard (MkPlayerCard (toCard h).id other (Just h.owner))]]
      _ -> pure []
  CreatePowerOfMordorDeck -> Just $ one $ mechanic [ikeyPart "log.powerOfMordorDeckCreated" []]
  RevealTopOfPowerOfMordorDeck -> Just $ one $ toned Bad $ mechanic [ikeyPart "log.powerOfMordorReveals" []]
  -- an objective ally turning to its enemy side, as Captain Sahír's
  Flip (AllyTarget aid) -> Just $ whenRef (toTarget aid) \a -> one $ mechanic [ikeyPart "log.allyFlips" ["ally" ~> a]]
  -- a location turns over, as an Uncharted one does: the side it turns to is named
  Flip (LocationTarget lid) -> Just do
    ls <- getsGame (entitiesLocations . gameEntities)
    case Map.lookup lid ls of
      Just l | Just other <- (toAttrs l).otherSide -> whenRef (toTarget lid) \from ->
        one $ mechanic [ikeyPart "log.locationFlips" ["location" ~> from, "card" ~> EncounterCard (MkEncounterCard (toCard l).id other)]]
      _ -> pure []
  -- what is seen stays with the player who looked
  LookAtFacedownSide pid lid -> Just $ whenRef (toTarget lid) \l -> do
    who <- playerRefFor pid
    one $ mechanic [ikeyPart "log.looksAtFacedownSide" ["player" ~> who, "location" ~> l]]
  ShuffleIntoUnchartedDeck lid -> Just $ whenRef (toTarget lid) \l -> one $ mechanic [ikeyPart "log.shuffledIntoUnchartedDeck" ["location" ~> l]]
  ShuffleUnchartedLocations -> Just $ one $ mechanic [ikeyPart "log.unchartedShuffled" []]
  -- Shrine to Morgoth floods, its progress kept: narrated as any location turning over
  FlipKeepingProgress lid -> narrationFor (Flip $ LocationTarget lid)
  ShuffleIntoGrottoDeck lid -> Just $ whenRef (toTarget lid) \l -> one $ mechanic [ikeyPart "log.shuffledIntoGrottoDeck" ["location" ~> l]]
  PrepareIslandMap cards -> Just $ one $ mechanic [ikeyPart "log.islandMapPrepared" ["count" ~> length cards]]
  RevealShadowCard eid cid -> Just $ shadowFaceUp eid cid
  TurnShadowCardFaceUp eid cid -> Just $ shadowFaceUp eid cid
  AttachTreachery tid target -> Just $ attached (toTarget tid) target
  -- a Grapple enemy attaching facedown to its location, and letting go of it (The Thing
  -- in the Depths)
  PlaceEnemy eid (AttachedTo grappled@(LocationTarget _)) -> Just $ whenRef (toTarget eid) \e -> whenRef grappled \l ->
    one $ toned Bad $ mechanic [ikeyPart "log.grapples" ["enemy" ~> e, "location" ~> l]]
  PlaceEnemy eid StagingArea -> Just do
    enemies <- getsGame (entitiesEnemies . gameEntities)
    case (.placement) <$> Map.lookup eid enemies of
      Just (AttachedTo grappled@(LocationTarget _)) -> whenRef (toTarget eid) \e -> whenRef grappled \l ->
        one $ mechanic [ikeyPart "log.letsGo" ["enemy" ~> e, "location" ~> l]]
      _ -> pure []
  PlaceEnemy eid (AttachedTo target) -> Just $ attached (toTarget eid) target
  PlaceAlly aid (AttachedTo target) -> Just $ attached (toTarget aid) target
  AttachCard aid target -> Just $ attached (toTarget aid) target
  TakePrisoner hid -> Just $ whenRef (toTarget hid) \h -> one $ toned Bad $ mechanic [ikeyPart "log.takenPrisoner" ["hero" ~> h]]
  RescuePrisoner hid -> Just $ whenRef (toTarget hid) \h -> one $ toned Good $ mechanic [ikeyPart "log.rescued" ["hero" ~> h]]
  GuardCharacter c cid -> Just $ attached c (CardIdTarget cid)
  GuardCard card cid -> Just $ whenRef (CardIdTarget cid) \h -> one $ mechanic [ikeyPart "log.attachedTo" ["card" ~> card, "target" ~> h]]
  RescueCharacter c -> Just $ whenRef c \h -> one $ toned Good $ mechanic [ikeyPart "log.rescued" ["hero" ~> h]]
  MoveHeroToStagingArea hid -> Just $ whenRef (toTarget hid) \h -> one $ toned Bad $ mechanic [ikeyPart "log.heroToStagingArea" ["hero" ~> h]]
  TakeControlOfHero hid -> Just $ whenRef (toTarget hid) \h -> do
    owner <- getsGame (fmap (.owner) . Map.lookup hid . entitiesHeroes . gameEntities)
    flip (maybe (pure [])) owner \pid -> do
      who <- playerRefFor pid
      one $ toned Good $ mechanic [ikeyPart "log.takesControlOfHero" ["player" ~> who, "hero" ~> h]]
  PutObjectiveHeroIntoPlayUnder pid card -> Just do
    who <- playerRefFor pid
    one $ mechanic [ikeyPart "log.takesControlOfHero" ["player" ~> who, "hero" ~> EncounterCard card]]
  -- an enemy taking an objective, as Alcaron's Scroll goes to the enemy that damaged its hero
  ClaimObjective oid target@(EnemyTarget _) -> Just $ attached (toTarget oid) target
  ClaimObjective oid target -> Just $ whenRef (toTarget oid) \o -> whenRef target \t ->
    one $ toned Good $ mechanic [ikeyPart "log.claims" ["target" ~> t, "card" ~> o]]
  ResolveArchery -> Just do
    total <- getArcheryTotal
    pure [toned Bad $ mechanic [ikeyPart "log.archery" ["count" ~> total]] | total > 0]
  -- \* The quest
  QuestResolution -> Just do
    willpower <- getCommittedWillpower
    (name, _) <- questingStat
    threat <- getStagingThreat
    let versus =
          ikeyPart "log.versus" ["willpower" ~> stat name willpower, "threat" ~> stat "threat" threat]
    one $ case compare willpower threat of
      GT ->
        toned Good
          $ test [ikeyPart "log.questSucceeds" ["progress" ~> stat "progress" (willpower - threat)], versus]
      LT -> toned Bad $ test [ikeyPart "log.questFails" ["count" ~> (threat - willpower)], versus]
      EQ -> test [ikeyPart "log.questTied" [], versus]
  ResolveEscapeTest t -> Just do
    (total, escape) <- escapeTestTotals t
    let versus = ikeyPart "log.escapeVersus" ["total" ~> total, "escape" ~> escape]
    one
      $ if escape > total
        then toned Bad $ test [ikeyPart "log.escapeFails" [], versus]
        else toned Good $ test [ikeyPart "log.escapePasses" [], versus]
  CompleteQuest qid -> Just $ whenRef (toTarget qid) \q -> one $ toned Good $ mechanic [ikeyPart "log.questComplete" ["quest" ~> q]]
  SetCurrentQuest t -> Just $ whenRef t \q -> one $ mechanic [ikeyPart "log.currentQuest" ["quest" ~> q]]
  ShuffleIntoQuestDeck card -> Just $ one $ mechanic [ikeyPart "log.shuffledIntoQuestDeck" ["card" ~> EncounterCard card]]
  PutOnBottomOfQuestDeck card -> Just $ one $ mechanic [ikeyPart "log.putOnBottomOfQuestDeck" ["card" ~> EncounterCard card]]
  RemoveFromQuestDeck card -> Just $ one $ toned Good $ mechanic [ikeyPart "log.removedFromQuestDeck" ["card" ~> EncounterCard card]]
  AdvanceToQuest card -> Just $ one $ structure [ikeyPart "log.questStage" ["quest" ~> EncounterCard card]]
  -- \* Hands and decks
  ShuffleCardIntoDeck pid card -> Just $ withPlayer pid \_ who -> one $ mechanic [ikeyPart "log.shuffledIntoPlayersDeck" ["card" ~> card, "player" ~> who]]
  DoDrawCards drawing | drawing.count > 0 -> Just $ withPlayer drawing.player \player who -> do
    let drawn = take drawing.count player.deck
    pure
      [ forPlayer player.id $ mechanic [ikeyPart "log.drawsCards" ["player" ~> who, "cards" ~> drawn]]
      , exceptPlayer player.id
          $ mechanic [ikeyPart "log.drawsCount" ["player" ~> who, "count" ~> length drawn]]
      ]
  TakeFromDeck pid cid -> Just $ withPlayer pid \player who ->
    pure
      $ concat
        [ [ forPlayer pid $ mechanic [ikeyPart "log.takesIntoHand" ["player" ~> who, "card" ~> card]]
          , exceptPlayer pid $ mechanic [ikeyPart "log.takesCardIntoHand" ["player" ~> who]]
          ]
        | card <- filter ((== cid) . (.id)) player.deck
        ]
  DiscardFromHand pid cid -> Just $ withPlayer pid \player who ->
    pure
      [ mechanic [ikeyPart "log.discardsFromHand" ["player" ~> who, "card" ~> card]]
      | card <- filter ((== cid) . (.id)) (discardableHand player)
      ]
  DiscardRandomFromHand pid n | n > 0 -> Just $ withPlayer pid \_ who ->
    one $ mechanic [ikeyPart "log.discardsAtRandom" ["player" ~> who, "count" ~> n]]
  ReturnFromDiscardToHand pid cid -> Just $ withPlayer pid \player who ->
    pure
      [ mechanic [ikeyPart "log.returnsFromDiscard" ["player" ~> who, "card" ~> card]]
      | card <- filter ((== cid) . (.id)) player.discard
      ]
  ShuffleDiscardIntoDeck pid -> Just $ withPlayer pid \_ who -> one $ mechanic [ikeyPart "log.shufflesDiscardIntoDeck" ["player" ~> who]]
  ShuffleTopOfDiscardIntoDeck pid n -> Just $ withPlayer pid \player who ->
    let moved = min n (length player.discard)
     in if moved > 0 then one $ mechanic [ikeyPart "log.shufflesTopOfDiscardIntoDeck" ["player" ~> who, "count" ~> moved]] else pure []
  MoveToBottomOfDeck pid cid -> Just $ withPlayer pid \player who ->
    pure
      [ mechanic [ikeyPart "log.movesToBottomOfDeck" ["player" ~> who, "card" ~> card]]
      | card <- filter ((== cid) . (.id)) player.deck
      ]
  PutInHand card | Just pid <- card.owner -> Just do
    who <- playerRefFor pid
    one $ mechanic [ikeyPart "log.takesIntoHand" ["player" ~> who, "card" ~> PlayerCard card]]
  PlayAtNoCost _ card -> Just $ one $ mechanic [ikeyPart "log.playedAtNoCost" ["card" ~> PlayerCard card]]
  -- \* The Stormcaller's area and its quest stage (Flight of the Stormcaller)
  PlaceEnemy eid StormcallerArea -> Just $ whenRef (toTarget eid) \e -> one $ mechanic [ikeyPart "log.toStormcallerArea" ["card" ~> e]]
  PlaceLocation lid StormcallerArea -> Just $ whenRef (toTarget lid) \l -> one $ mechanic [ikeyPart "log.toStormcallerArea" ["card" ~> l]]
  ScenarioSpecific "stormcaller.progress" (parseMaybe parseJSON -> Just (n :: Int)) ->
    Just $ one $ toned Bad $ mechanic [ikeyPart "log.stormcallerProgress" ["count" ~> n]]
  ScenarioSpecific "stormcaller.advanced" (parseMaybe parseJSON -> Just card) ->
    Just $ one $ toned Bad $ structure [ikeyPart "log.stormcallerAdvances" ["quest" ~> EncounterCard card]]
  ScenarioSpecific "stormcaller.join" _ -> Just $ one $ toned Good $ mechanic [ikeyPart "log.stormcallerCaughtUp" []]
  -- \* Cold-Drake set aside with its damage (The Withered Heath)
  ScenarioSpecific "witheredHeath.drakeHealed" (parseMaybe parseJSON -> Just (n :: Int)) ->
    Just $ one $ toned Good $ mechanic [ikeyPart "log.coldDrakeHealed" ["count" ~> n]]
  -- \* The Orcs' area and their quest stage (Race Across Harad)
  PlaceEnemy eid OrcsArea -> Just $ whenRef (toTarget eid) \e -> one $ mechanic [ikeyPart "log.toOrcsArea" ["card" ~> e]]
  PlaceLocation lid OrcsArea -> Just $ whenRef (toTarget lid) \l -> one $ mechanic [ikeyPart "log.toOrcsArea" ["card" ~> l]]
  ScenarioSpecific "orcs.progress" (parseMaybe parseJSON -> Just (n :: Int)) ->
    Just $ one $ toned Bad $ mechanic [ikeyPart "log.orcsProgress" ["count" ~> n]]
  ScenarioSpecific "orcs.advanced" (parseMaybe parseJSON -> Just card) ->
    Just $ one $ toned Bad $ structure [ikeyPart "log.orcsAdvance" ["quest" ~> EncounterCard card]]
  ScenarioSpecific "orcs.join" _ -> Just $ one $ toned Bad $ mechanic [ikeyPart "log.orcsCaughtUp" []]
  ScenarioSpecific "orcs.split" _ -> Just $ one $ toned Good $ mechanic [ikeyPart "log.orcsLeftBehind" []]
  -- \* Locations the players control, as Osgiliath's
  PlaceLocation lid placement -> Just $ whenRef (toTarget lid) \l -> do
    from <- maybe Unplaced (.placement) . Map.lookup lid <$> getsGame (entitiesLocations . gameEntities)
    case (from, placement) of
      (_, InPlayArea pid) | from /= placement -> takesControl pid l
      (InPlayArea _, StagingArea) -> one $ toned Bad $ mechanic [ikeyPart "log.returnedToStaging" ["location" ~> l]]
      _ -> pure []
  PutLocationIntoPlay card (InPlayArea pid) -> Just $ takesControl pid (EncounterCard card)
  ScenarioSpecific "scour" (parseMaybe parseJSON -> Just target) -> Just $ whenRef target \t ->
    one $ toned Bad $ mechanic [ikeyPart "log.scour" ["target" ~> t]]
  -- Escape from Mount Gram's captured cards, rescued from underneath the card
  ScenarioSpecific "mountGram.rescue" (parseMaybe parseJSON -> Just (host :: Card, cards :: [PlayerCard])) ->
    Just $ one $ toned Good $ mechanic [ikeyPart "log.capturedRescued" ["card" ~> host, "count" ~> length cards]]
  -- a location leaving play for underneath a card, as one Aflame burns down under The
  -- Havens Burn
  PlaceUnderneath target [card] | cdCardType (toCardDef card) == LocationType -> Just $ whenRef target \t ->
    one $ toned Bad $ mechanic [ikeyPart "log.locationPlacedUnderneath" ["location" ~> card, "target" ~> t]]
  -- an enemy leaving play for underneath a card, as Narrow Alleyway hides one; one from
  -- out of play goes facedown unnamed, as The Steward's Fear's underworld cards
  PlaceUnderneath target [card] | cdCardType (toCardDef card) == EnemyType -> Just $ whenRef target \t -> do
    inPlay <- getsGame (any ((== card.id) . (.id) . toCard) . entitiesEnemies . gameEntities)
    one
      $ if inPlay
        then mechanic [ikeyPart "log.enemyPlacedUnderneath" ["enemy" ~> card, "target" ~> t]]
        else toned Bad $ mechanic [ikeyPart "log.placedUnderneath" ["target" ~> t, "count" ~> (1 :: Int)]]
  -- a hero card attached faceup to an attachment, as Thorongil takes one
  PlaceUnderneath target@(AttachmentTarget _) [card] | cdCardType (toCardDef card) == HeroType -> Just $ whenRef target \t ->
    one $ mechanic [ikeyPart "log.heroCardAttachedTo" ["card" ~> card, "target" ~> t]]
  PlaceUnderneath target cards | notNull cards -> Just $ whenRef target \t ->
    one $ toned Bad $ mechanic [ikeyPart "log.placedUnderneath" ["target" ~> t, "count" ~> length cards]]
  _ -> Nothing
 where
  takesControl :: (HasGame m, ToLogPart a) => PlayerId -> a -> m [LogEntry]
  takesControl pid l = do
    who <- playerRefFor pid
    one $ toned Good $ mechanic [ikeyPart "log.takesControl" ["player" ~> who, "location" ~> l]]

one :: Applicative m => LogEntry -> m [LogEntry]
one = pure . pure

-- | Say something only when the target can be named.
whenRef :: HasGame m => Target -> (LogRef -> m [LogEntry]) -> m [LogEntry]
whenRef target f = targetRefFor target >>= maybe (pure []) f

-- | A shadow card dealt to the enemy turning face up; one dealt face up, as Silver Lamp
-- deals it, was told then.
shadowFaceUp :: HasGame m => EnemyId -> CardId -> m [LogEntry]
shadowFaceUp eid cid = whenRef (toTarget eid) \e -> do
  mEnemy <- getsGame (Map.lookup eid . entitiesEnemies . gameEntities)
  pure
    [ mechanic [ikeyPart key ["enemy" ~> e, "card" ~> EncounterCard card]]
    | Just en <- [mEnemy]
    , cid `notElem` (toAttrs en).enemyShadowRevealed
    , card <- take 1 $ filter ((== cid) . (.id)) en.shadowCards
    , let key = if cdShadowEffect (toCardDef card) then "log.shadowRevealed" else "log.shadowRevealedNoEffect"
    ]

-- | Say something only while the card is still hidden in the player's play area.
whenHidden :: HasGame m => PlayerId -> EncounterCard -> (LogRef -> m [LogEntry]) -> m [LogEntry]
whenHidden pid card f = do
  hidden <- getsGame (Map.findWithDefault [] pid . (.hiddenCards) . toAttrs . gameScenario)
  if card `elem` hidden then f =<< playerRefFor pid else pure []

withPlayer :: HasGame m => PlayerId -> (Player -> LogRef -> m [LogEntry]) -> m [LogEntry]
withPlayer pid f = do
  players <- getsGame gamePlayers
  maybe (pure []) (\p -> f p =<< playerRefFor pid) (Map.lookup pid players)

-- | Say it only while a card is being played or its ability used.
duringCard :: HasGame m => m [LogEntry] -> m [LogEntry]
duringCard say = do
  open <- getsGame gameLogBlocks
  case (.logBlockKind) <$> lastMay open of
    Just (AbilityBlock {}) -> say
    Just (PlayBlock {}) -> say
    _ -> pure []

simply :: HasGame m => Text -> Target -> m [LogEntry]
simply key target = whenRef target \t -> one $ mechanic [ikeyPart key ["target" ~> t]]

attached :: HasGame m => Target -> Target -> m [LogEntry]
attached card host = whenRef card \c -> whenRef host \h -> one $ mechanic [ikeyPart "log.attachedTo" ["card" ~> c, "target" ~> h]]

threatChange :: HasGame m => Text -> PlayerId -> Int -> (Int -> Int) -> m [LogEntry]
threatChange key pid n change = withPlayer pid \player who ->
  one $ mechanic [ikeyPart key ["player" ~> who, "count" ~> n, "total" ~> change player.threat]]

-- | A block's header: what is happening, named before it does.
openBlock :: HasGame m => LogBlock -> m [LogEntry]
openBlock block = do
  open <- getsGame gameLogBlocks
  let gid = fromMaybe (logBlockKey block) $ logBlockPath (open <> [block])
      header = one . opensGroup gid
  case block.logBlockKind of
    PlayBlock pid card -> do
      who <- playerRefFor pid
      header $ action [ikeyPart "log.plays" ["player" ~> who, "card" ~> card]]
    AbilityBlock pid source forced -> do
      mCard <- sourceRefFor source
      who <- playerRefFor pid
      let card = fromMaybe who mCard
      header
        $ if forced
          then mechanic [ikeyPart "log.forced" ["card" ~> card]]
          else action [ikeyPart "log.usesAbility" ["player" ~> who, "card" ~> card]]
    RevealBlock card -> do
      let def = toCardDef card
      header
        $ mechanic
          [ case cdThreat def of
              Just threat
                | cdCardType def `elem` [EnemyType, LocationType] ->
                    ikeyPart "log.revealedThreat" ["card" ~> EncounterCard card, "threat" ~> stat "threat" threat]
              _ -> ikeyPart "log.revealed" ["card" ~> EncounterCard card]
          ]
    WhenRevealedBlock card ->
      header $ mechanic [ikeyPart "log.resolvedWhenRevealed" ["card" ~> EncounterCard card]]
    EnemyAttackBlock pid eid -> whenRef (toTarget eid) \e -> do
      who <- playerRefFor pid
      attack <- getEnemyAttack eid
      header
        $ mechanic
          [ikeyPart "log.enemyAttacks" ["enemy" ~> e, "player" ~> who, "attack" ~> stat "attack" attack]]
    PlayerAttackBlock _ eid attackers -> do
      enemies <- getsGame (entitiesEnemies . gameEntities)
      case Map.lookup eid enemies of
        Nothing -> pure []
        Just en -> do
          refs <- catMaybes <$> traverse targetRefFor attackers
          attack <- getAttackStrength attackers eid (toCardDef en)
          defense <- getEnemyDefense eid
          header
            $ action
              [ ikeyPart
                  "log.attacks"
                  [ "attackers" ~> refs
                  , "enemy" ~> entityRef RefEnemy eid en
                  , "attack" ~> stat "attack" attack
                  , "defense" ~> stat "defense" defense
                  ]
              ]

-- | A block's summary, where its outcome is worth a band of its own.
closeBlock :: HasGame m => UUID -> m [LogEntry]
closeBlock bid = do
  open <- getsGame gameLogBlocks
  case break ((== bid) . (.id)) open of
    (outer, block : _) | Just gid <- logBlockPath (outer <> [block]) -> case block.logBlockKind of
      PlayerAttackBlock _ eid _ -> do
        enemies <- getsGame (entitiesEnemies . gameEntities)
        -- destroyed by the attack's damage, not gone some other way during it
        destroyed <- getsGame (any ((== EnemyTarget eid) . (.target)) . gameAttackDestroyed)
        case Map.lookup eid enemies of
          _ | destroyed -> pure [closesGroup gid $ toned Good $ test [ikeyPart "log.attackDestroys" []]]
          Nothing -> pure []
          Just en -> do
            hitPoints <- getEnemyHitPoints eid
            let e = entityRef RefEnemy eid en
            pure
              [ closesGroup gid
                  $ test [ikeyPart "log.attackDamages" ["enemy" ~> e, "damage" ~> en.damage, "hitPoints" ~> hitPoints]]
              ]
      _ -> pure []
    _ -> pure []

-- | The heading's setting, named.
headingPart :: Heading -> LogPart
headingPart h = ikeyPart ("log.heading." <> toLower (tshow h)) []
