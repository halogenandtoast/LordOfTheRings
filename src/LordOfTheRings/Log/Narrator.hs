-- | The game log, derived from the message stream, as in Arkham's "Arkham.Log.Narrator".
--
-- Cards do not write the log. Every change to the game passes through a 'Message', so
-- one 'case' here, knowing what each message means, gives the whole log in one place a
-- reader can review, and a new card logs correctly without doing anything.
--
-- Three rules:
--
-- 1. __Silence is the default.__ Most messages are plumbing (windows, asks, steps), and a
--    message nothing here claims says nothing.
-- 2. __The narrator runs before the message.__ It reads the game as the message finds it:
--    a card about to leave play is still there to be named, a count is still the old one.
-- 3. __Never break the game.__ It only reads, never pushes, and never assumes something
--    named in a message is still in play.
module LordOfTheRings.Log.Narrator (narrationFor) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities
import LordOfTheRings.Game.Base
import LordOfTheRings.Helpers.Character
import LordOfTheRings.Helpers.Staging
import LordOfTheRings.Id
import LordOfTheRings.Log
import LordOfTheRings.Log.Block
import LordOfTheRings.Log.Refs
import LordOfTheRings.Matcher (getDamage)
import LordOfTheRings.Message
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Placement
import LordOfTheRings.Player.Types
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token

-- | What a message adds to the log, if anything. The match is pure, so a message that
-- says nothing costs nothing; only a narrating one reads the game.
narrationFor :: (HasGame m) => Message -> Maybe (m [LogEntry])
narrationFor = \case
  -- * Structure: the headings a reader finds their way by
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
  -- * Blocks: an event that spans several answers, as one entry
  OpenLogBlock block -> Just $ openBlock block
  CloseLogBlock bid -> Just $ closeBlock bid
  -- * What the players do
  PutAllyIntoPlay pid cid -> Just do
    who <- playerRefFor pid
    card <- targetRefFor (CardIdTarget cid)
    pure [action [ikeyPart "log.putsIntoPlay" ["player" ~> who, "card" ~> c]] | Just c <- [card]]
  PutAllyIntoPlayUnder _ card -> Just $ one $ mechanic [ikeyPart "log.putIntoPlay" ["card" ~> PlayerCard card]]
  PutHeroIntoPlay _ card -> Just $ one $ mechanic [ikeyPart "log.putIntoPlay" ["card" ~> PlayerCard card]]
  CommitToQuest target -> Just $ whenRef target \c -> do
    willpower <- getWillpower target
    one $ action [ikeyPart "log.commits" ["character" ~> c, "willpower" ~> stat "willpower" willpower]]
  TravelTo lid -> Just $ whenRef (toTarget lid) \l -> one $ action [ikeyPart "log.travels" ["location" ~> l]]
  EngageEnemy pid eid -> Just $ whenRef (toTarget eid) \e -> do
    who <- playerRefFor pid
    one $ action [ikeyPart "log.engages" ["enemy" ~> e, "player" ~> who]]
  DefendWith _ _ defender -> Just $ whenRef defender \d -> one $ action [ikeyPart "log.defends" ["defender" ~> d]]
  DefendedByMany _ _ defenders@(_ : _ : _) -> Just do
    refs <- catMaybes <$> traverse targetRefFor defenders
    one $ action [ikeyPart "log.defendMany" ["defenders" ~> refs]]
  UndefendedAttack pid _ -> Just do
    who <- playerRefFor pid
    one $ toned Bad $ action [ikeyPart "log.undefended" ["player" ~> who]]
  Mulligan pid -> Just do
    who <- playerRefFor pid
    one $ action [ikeyPart "log.mulligan" ["player" ~> who]]
  -- * Threat
  RaiseThreat pid n | n > 0 -> Just $ threatChange "log.threatRaised" pid n (+ n)
  LowerThreat pid n | n > 0 -> Just $ threatChange "log.threatLowered" pid n (max 0 . subtract n)
  EliminatePlayer pid -> Just do
    who <- playerRefFor pid
    one $ toned Bad $ mechanic [ikeyPart "log.eliminated" ["player" ~> who]]
  -- * Damage and tokens
  DealDamage source target n | n > 0 -> Just $ whenRef target \t -> do
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
      -- dealt with as damage, where the source is known
      Damage -> pure []
   where
    tokens key = whenRef target \t -> one $ mechanic [ikeyPart key ["target" ~> t, "count" ~> n]]
  RemoveTokens _ target token n | n > 0 -> case token of
    Progress -> Just $ whenRef target \t -> one $ mechanic [ikeyPart "log.progressRemoved" ["target" ~> t, "progress" ~> stat "progress" n]]
    Resource -> Just $ whenRef target \t -> one $ notice [ikeyPart "log.spendsResources" ["target" ~> t, "count" ~> n]]
    Damage -> Nothing
  -- what a card does to characters; elsewhere readying and exhausting are routine
  Ready target -> Just $ duringCard $ simply "log.readies" target
  Exhaust target -> Just $ duringCard $ simply "log.exhausts" target
  -- * Leaving play
  Destroy target -> Just $ whenRef target \t ->
    -- a character going down is bad; an enemy going down is the point
    one $ toned (case target of EnemyTarget _ -> Good; _ -> Bad) $ mechanic [ikeyPart "log.destroyed" ["target" ~> t]]
  Discard target -> Just $ whenRef target \t -> do
    -- a destroyed character is discarded as it goes, and was already said
    destroyed <- case target of
      HeroTarget _ -> (>=) <$> getDamage target <*> getHitPoints target
      AllyTarget _ -> (>=) <$> getDamage target <*> getHitPoints target
      _ -> pure False
    resolved <- case target of
      -- a treachery that has resolved is discarded as a matter of course
      TreacheryTarget tid -> maybe True ((== Unplaced) . (.placement)) . Map.lookup tid <$> getsGame (entitiesTreacheries . gameEntities)
      _ -> pure False
    pure [mechanic [ikeyPart "log.discarded" ["target" ~> t]] | not (destroyed || resolved)]
  ReturnToHand target -> Just $ simply "log.returnsToHand" target
  ShuffleIntoOwnersDeck target -> Just $ simply "log.shuffledIntoOwnersDeck" target
  ShuffleIntoEncounterDeck target -> Just $ simply "log.shuffledIntoEncounterDeck" target
  ReturnToTopOfEncounterDeck target -> Just $ simply "log.returnedToTopOfEncounterDeck" target
  -- * The encounter deck
  AddToStaging card -> Just $ one $ mechanic [ikeyPart "log.addedToStaging" ["card" ~> EncounterCard card]]
  PutObjectiveIntoPlay card -> Just $ one $ mechanic [ikeyPart "log.placedInStaging" ["card" ~> EncounterCard card]]
  EngageOrcGuard pid _ -> Just do
    who <- playerRefFor pid
    one $ mechanic [ikeyPart "log.orcGuardEngages" ["player" ~> who]]
  AttachShadowCard eid _ -> Just $ whenRef (toTarget eid) \e -> one $ mechanic [ikeyPart "log.dealtShadow" ["enemy" ~> e]]
  RevealShadowCard eid cid -> Just $ whenRef (toTarget eid) \e -> do
    mCard <- getsGame (\g -> find ((== cid) . (.id)) . (.shadowCards) =<< Map.lookup eid g.entities.enemies)
    pure
      [ mechanic [ikeyPart key ["enemy" ~> e, "card" ~> EncounterCard card]]
      | Just card <- [mCard]
      , let key = if cdShadowEffect (toCardDef card) then "log.shadowRevealed" else "log.shadowRevealedNoEffect"
      ]
  AttachTreachery tid target -> Just $ attached (toTarget tid) target
  AttachCard aid target -> Just $ attached (toTarget aid) target
  TakePrisoner hid -> Just $ whenRef (toTarget hid) \h -> one $ toned Bad $ mechanic [ikeyPart "log.takenPrisoner" ["hero" ~> h]]
  RescuePrisoner hid -> Just $ whenRef (toTarget hid) \h -> one $ toned Good $ mechanic [ikeyPart "log.rescued" ["hero" ~> h]]
  ClaimObjective oid target -> Just $ whenRef (toTarget oid) \o -> whenRef target \t ->
    one $ toned Good $ mechanic [ikeyPart "log.claims" ["target" ~> t, "card" ~> o]]
  -- * The quest
  QuestResolution -> Just do
    willpower <- getCommittedWillpower
    threat <- getStagingThreat
    let versus = ikeyPart "log.versus" ["willpower" ~> stat "willpower" willpower, "threat" ~> stat "threat" threat]
    one $ case compare willpower threat of
      GT -> toned Good $ test [ikeyPart "log.questSucceeds" ["progress" ~> stat "progress" (willpower - threat)], versus]
      LT -> toned Bad $ test [ikeyPart "log.questFails" ["count" ~> (threat - willpower)], versus]
      EQ -> test [ikeyPart "log.questTied" [], versus]
  ResolveEscapeTest t -> Just do
    (total, escape) <- escapeTestTotals t
    let versus = ikeyPart "log.escapeVersus" ["total" ~> total, "escape" ~> escape]
    one $ if escape > total
      then toned Bad $ test [ikeyPart "log.escapeFails" [], versus]
      else toned Good $ test [ikeyPart "log.escapePasses" [], versus]
  CompleteQuest qid -> Just $ whenRef (toTarget qid) \q -> one $ toned Good $ mechanic [ikeyPart "log.questComplete" ["quest" ~> q]]
  AdvanceToQuest card -> Just $ one $ structure [ikeyPart "log.questStage" ["quest" ~> EncounterCard card]]
  -- * Hands and decks
  DrawCards drawing | drawing.count > 0 -> Just $ withPlayer drawing.player \player who -> do
    let drawn = take drawing.count player.deck
    pure
      [ forPlayer player.id $ mechanic [ikeyPart "log.drawsCards" ["player" ~> who, "cards" ~> drawn]]
      , exceptPlayer player.id $ mechanic [ikeyPart "log.drawsCount" ["player" ~> who, "count" ~> length drawn]]
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
    pure [mechanic [ikeyPart "log.discardsFromHand" ["player" ~> who, "card" ~> card]] | card <- filter ((== cid) . (.id)) player.hand]
  DiscardRandomFromHand pid n | n > 0 -> Just $ withPlayer pid \_ who ->
    one $ mechanic [ikeyPart "log.discardsAtRandom" ["player" ~> who, "count" ~> n]]
  ReturnFromDiscardToHand pid cid -> Just $ withPlayer pid \player who ->
    pure [mechanic [ikeyPart "log.returnsFromDiscard" ["player" ~> who, "card" ~> card]] | card <- filter ((== cid) . (.id)) player.discard]
  ShuffleDiscardIntoDeck pid -> Just $ withPlayer pid \_ who -> one $ mechanic [ikeyPart "log.shufflesDiscardIntoDeck" ["player" ~> who]]
  _ -> Nothing

one :: (Applicative m) => LogEntry -> m [LogEntry]
one = pure . pure

-- | Say something only when the target can be named.
whenRef :: (HasGame m) => Target -> (LogRef -> m [LogEntry]) -> m [LogEntry]
whenRef target f = targetRefFor target >>= maybe (pure []) f

withPlayer :: (HasGame m) => PlayerId -> (Player -> LogRef -> m [LogEntry]) -> m [LogEntry]
withPlayer pid f = do
  players <- getsGame gamePlayers
  maybe (pure []) (\p -> f p =<< playerRefFor pid) (Map.lookup pid players)

-- | Say it only while a card is being played or its ability used.
duringCard :: (HasGame m) => m [LogEntry] -> m [LogEntry]
duringCard say = do
  open <- getsGame gameLogBlocks
  case (.logBlockKind) <$> lastMay open of
    Just (AbilityBlock {}) -> say
    Just (PlayBlock {}) -> say
    _ -> pure []

simply :: (HasGame m) => Text -> Target -> m [LogEntry]
simply key target = whenRef target \t -> one $ mechanic [ikeyPart key ["target" ~> t]]

attached :: (HasGame m) => Target -> Target -> m [LogEntry]
attached card host = whenRef card \c -> whenRef host \h -> one $ mechanic [ikeyPart "log.attachedTo" ["card" ~> c, "target" ~> h]]

threatChange :: (HasGame m) => Text -> PlayerId -> Int -> (Int -> Int) -> m [LogEntry]
threatChange key pid n change = withPlayer pid \player who ->
  one $ mechanic [ikeyPart key ["player" ~> who, "count" ~> n, "total" ~> change player.threat]]

-- | A block's header: what is happening, named before it does.
openBlock :: (HasGame m) => LogBlock -> m [LogEntry]
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
              Just threat | cdCardType def `elem` [EnemyType, LocationType] -> ikeyPart "log.revealedThreat" ["card" ~> EncounterCard card, "threat" ~> stat "threat" threat]
              _ -> ikeyPart "log.revealed" ["card" ~> EncounterCard card]
          ]
    EnemyAttackBlock pid eid -> whenRef (toTarget eid) \e -> do
      who <- playerRefFor pid
      attack <- getEnemyAttack eid
      header $ mechanic [ikeyPart "log.enemyAttacks" ["enemy" ~> e, "player" ~> who, "attack" ~> stat "attack" attack]]
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
                  ["attackers" ~> refs, "enemy" ~> entityRef RefEnemy eid en, "attack" ~> stat "attack" attack, "defense" ~> stat "defense" defense]
              ]

-- | A block's summary, where its outcome is worth a band of its own.
closeBlock :: (HasGame m) => UUID -> m [LogEntry]
closeBlock bid = do
  open <- getsGame gameLogBlocks
  case break ((== bid) . (.id)) open of
    (outer, block : _) | Just gid <- logBlockPath (outer <> [block]) -> case block.logBlockKind of
      PlayerAttackBlock _ eid _ -> do
        enemies <- getsGame (entitiesEnemies . gameEntities)
        case Map.lookup eid enemies of
          Nothing -> pure [closesGroup gid $ toned Good $ test [ikeyPart "log.attackDestroys" []]]
          Just en -> do
            hitPoints <- getEnemyHitPoints eid
            let e = entityRef RefEnemy eid en
            pure [closesGroup gid $ test [ikeyPart "log.attackDamages" ["enemy" ~> e, "damage" ~> en.damage, "hitPoints" ~> hitPoints]]]
      _ -> pure []
    _ -> pure []
