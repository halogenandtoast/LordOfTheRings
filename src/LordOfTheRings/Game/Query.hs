module LordOfTheRings.Game.Query (module LordOfTheRings.Game.Query) where

import Data.Map.Strict qualified as Map (findWithDefault, member)
import LordOfTheRings.Attachment (
  AttachmentAttrs (attachmentPlacement),
 )
import LordOfTheRings.Card (
  CardDef (cdKeywords),
  EncounterCard,
  HasCardDef (..),
 )
import LordOfTheRings.Classes.Entity (Entity (overAttrs))
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Contract (ContractAttrs (contractPlacement))
import LordOfTheRings.Effect (Effect (effectTarget))
import LordOfTheRings.Entities (
  attachmentsL,
  contractsL,
  objectivesL,
  treacheriesL,
 )
import LordOfTheRings.Game.Base (
  Game (gameModifiers, gamePlayers),
  effectsL,
  entitiesL,
 )
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationQuestPoints)
import LordOfTheRings.I18n (ikey)
import LordOfTheRings.Id (LocationId, PlayerId, QuestId)
import LordOfTheRings.Keyword (
  Keyword (Doomed, Guarded, GuardedBy),
 )
import LordOfTheRings.Matcher (
  LocationMatcher (LocationInStagingArea),
  Query (select),
  getLead,
  getLocation,
 )
import LordOfTheRings.Message (
  Message (
    Discard,
    DoStep,
    GuardLocation,
    GuardLocationBy,
    OptionalEngagement,
    PlaceTokens,
    RevealCard,
    RevealCardEngaged,
    Run
  ),
  questionLabelWith,
 )
import LordOfTheRings.Modifier (
  Modifier (modifierType),
  ModifierType (
    DamageNextRevealedEnemy,
    DiscardNextRevealed,
    DiscoverValueModifier,
    MayTakeQuestingProgress,
    ProgressPlacedOnInstead,
    TakesProgressBeforeActiveLocation,
    TakesProgressBeforeQuest
  ),
 )
import LordOfTheRings.Objective (
  ObjectiveAttrs (objectivePlacement),
 )
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Player.Runner (Player)
import LordOfTheRings.Prelude hiding (first, many)
import LordOfTheRings.Question (targetLabel)
import LordOfTheRings.Source (Source (GameSource, PlayerAttackSource))
import LordOfTheRings.Target (
  Target (
    AllyTarget,
    EnemyTarget,
    HeroTarget,
    LocationTarget,
    QuestTarget
  ),
  Targetable (toTarget),
 )
import LordOfTheRings.Token (Token (Progress))
import LordOfTheRings.Treachery (
  TreacheryAttrs (treacheryPlacement),
 )

import LordOfTheRings.Game.Cards (freeGuarded)

discardAttachedToQuest :: Game -> QuestId -> [Message]
discardAttachedToQuest g qid =
  [Discard GameSource (toTarget t) | t <- toList g.entities.treacheries, t.placement == AttachedTo (QuestTarget qid)]
    <> [Discard GameSource (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo (QuestTarget qid)]
    <> freeGuarded g (QuestTarget qid)

-- | What was attached to, or lasting on, a character is on the one it became.
retargetCharacter :: Target -> Target -> Game -> Game
retargetCharacter old new g =
  g
    & entitiesL . attachmentsL . traverse %~ (\a -> if a.placement == AttachedTo old then overAttrs (\x -> x {attachmentPlacement = AttachedTo new}) a else a)
    & entitiesL . treacheriesL . traverse %~ (\t -> if t.placement == AttachedTo old then overAttrs (\x -> x {treacheryPlacement = AttachedTo new}) t else t)
    & entitiesL . objectivesL . traverse %~ (\o -> if o.placement == AttachedTo old then overAttrs (\x -> x {objectivePlacement = AttachedTo new}) o else o)
    & entitiesL . contractsL . traverse %~ (\c -> if c.placement == AttachedTo old then overAttrs (\x -> x {contractPlacement = AttachedTo new}) c else c)
    & effectsL . traverse %~ (\e -> if effectTarget e == old then e {effectTarget = new} else e)

-- | Whether the source is a player's attack on an enemy, its damage.
isPlayerAttack :: Source -> Bool
isPlayerAttack = \case
  PlayerAttackSource _ -> True
  _ -> False

-- | Whether a character is still in play.
-- | Where progress placed on the card goes instead, as Blinding Blizzard takes it.
progressRedirect :: Game -> Target -> Maybe Target
progressRedirect g target =
  find
    (\t -> t /= target && inPlay g t)
    [t | ProgressPlacedOnInstead t <- map modifierType (Map.findWithDefault [] target (gameModifiers g))]

inPlay :: Game -> Target -> Bool
inPlay g = \case
  HeroTarget hid -> Map.member hid g.entities.heroes
  AllyTarget aid -> Map.member aid g.entities.allies
  EnemyTarget eid -> Map.member eid g.entities.enemies
  LocationTarget lid -> Map.member lid g.entities.locations
  _ -> True

-- | Put the card's cost up to be paid, less what the host it was chosen for takes off.
getPlayers :: HasGame m => m [Player]
getPlayers = toList <$> getsGame gamePlayers

printedDoomed :: HasCardDef a => a -> Int
printedDoomed a = sum [n | Doomed n <- toList (cdKeywords $ toCardDef a)]

-- | The player an optional engagement asks, and whether it is the first he is asked.
optionalEngagement :: Message -> Maybe (PlayerId, Bool)
optionalEngagement = \case
  OptionalEngagement pid -> Just (pid, True)
  DoStep 1 (OptionalEngagement pid) -> Just (pid, False)
  _ -> Nothing

-- | The card a message reveals, with the player it is put into play engaged with, if any.
revealedCard :: Message -> Maybe (Maybe PlayerId, EncounterCard)
revealedCard = \case
  RevealCard card -> Just (Nothing, card)
  RevealCardEngaged pid card -> Just (Just pid, card)
  _ -> Nothing

isDiscoverValueModifier :: ModifierType -> Bool
isDiscoverValueModifier = \case
  DiscoverValueModifier _ -> True
  _ -> False

isDamageNextRevealedEnemy :: ModifierType -> Bool
isDamageNextRevealedEnemy = \case
  DamageNextRevealedEnemy _ -> True
  _ -> False

isDiscardNextRevealed :: ModifierType -> Bool
isDiscardNextRevealed = \case
  DiscardNextRevealed _ -> True
  _ -> False

-- | Progress placed on a quest card or a location opens a window, as Pathless Country answers.
isQuestOrLocation :: Target -> Bool
isQuestOrLocation = \case
  QuestTarget _ -> True
  LocationTarget _ -> True
  _ -> False

-- | The locations in the staging area that take progress before the active location, as
-- Grim Foothills (and Mines of Moria, before the quest too), with the progress each still
-- takes.
takingProgressFirst :: GameT [(LocationId, Int)]
takingProgressFirst = do
  staged <- select LocationInStagingArea
  flip mapMaybeM staged \lid -> do
    first <- any (`elem` [TakesProgressBeforeActiveLocation, TakesProgressBeforeQuest]) <$> getModifiers lid
    l <- getLocation lid
    room <- subtract l.progress <$> getLocationQuestPoints lid
    pure $ guard (first && room > 0) $> (lid, room)

-- | The locations in the staging area that may take questing progress before the quest,
-- as Explorer's Almanac's, with the progress each still takes.
mayTakeQuestingProgress :: GameT [(LocationId, Int)]
mayTakeQuestingProgress = do
  staged <- select LocationInStagingArea
  flip mapMaybeM staged \lid -> do
    may <- elem MayTakeQuestingProgress <$> getModifiers lid
    l <- getLocation lid
    room <- subtract l.progress <$> getLocationQuestPoints lid
    pure $ guard (may && room > 0) $> (lid, room)

{- | Place the progress on one of the locations that take it first, the first player
choosing which, and the rest as the function says.
-}
progressFirst :: Source -> Int -> [(LocationId, Int)] -> (Int -> [Message]) -> GameT Message
progressFirst source n firsts rest = do
  lead <- getLead
  let place (lid, room) = let k = min n room in PlaceTokens source (toTarget lid) Progress k : rest (n - k)
  pure $ case firsts of
    [first] -> Run (place first)
    _ ->
      questionLabelWith (ikey "label.progressFirst") Nothing (map (toTarget . fst) firsts) lead
        $ [targetLabel lid (place first) | first@(lid, _) <- firsts]

-- | A location guarded as it is revealed: by the next card (Guarded), or by the first card
-- of its types discarded (Guarded (X)).
guardLocation :: LocationId -> CardDef -> [Message]
guardLocation lid def =
  [GuardLocation lid | Guarded `member` cdKeywords def] <> [GuardLocationBy lid ts | GuardedBy ts <- toList (cdKeywords def)]
