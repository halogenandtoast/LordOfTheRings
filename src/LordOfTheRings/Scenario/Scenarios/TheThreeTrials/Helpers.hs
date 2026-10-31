-- | What the cards of The Three Trials share: the Key objectives the players control, the
-- current trial's Key, and the set-aside Guardians, Keys and Barrows the trials draw on.
module LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers (
  Meta (..),
  getMeta,
  isKey,
  controlledKeys,
  keyHolder,
  currentKeyControlled,
  setCurrentKey,
  randomSetAside,
  setAsideFor,
  revealSetAside,
  revealFromVictoryDisplay,
  attachSetAsideKey,
  shuffleIntoBottomTen,
  engageGuardians,
  keyAbilities,
  runKey,
  runTrial,
  guardianAwake,
  spiritEngages,
  sharingClanTrait,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Ability
import LordOfTheRings.Card (CardCode, CardDef (..), EncounterCard (..), HasCardDef (..), toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.Claim (claimObjective)
import LordOfTheRings.Objective.Runner (Objective, ObjectiveAttrs (..))
import LordOfTheRings.Placement (Placement (AttachedTo), placementController)
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Runner (QuestAttrs (..))
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Timing (Timing (After))
import LordOfTheRings.Trait (Trait (Boar, Key, Raven, Wolf))

-- | The scenario's own state: the current trial's Key, by its card code.
newtype Meta = Meta {currentKey :: Maybe CardCode}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getMeta :: ScenarioAttrs -> Meta
getMeta attrs = fromMaybe (Meta Nothing) $ parseMaybe parseJSON attrs.meta

-- | The Traits a Guardian shares with its Key.
clanTraits :: [Trait]
clanTraits = [Boar, Wolf, Raven]

isKey :: HasCardDef a => a -> Bool
isKey = member Key . cdCardTraits . toCardDef

-- | The Key objectives the players control: those attached to their heroes.
controlledKeys :: HasGame m => m [Objective]
controlledKeys = filter onHero <$> objectivesWithTrait Key
 where
  onHero o = case o.placement of
    AttachedTo (HeroTarget _) -> True
    _ -> False

-- | The player who controls the Key sharing a Trait with the card.
keyHolder :: HasGame m => CardDef -> m (Maybe PlayerId)
keyHolder def = do
  held <- controlledKeys
  let shares o = any (\t -> t `member` cdCardTraits def && t `member` cdCardTraits (toCardDef o)) clanTraits
  case [hid | o <- held, shares o, AttachedTo (HeroTarget hid) <- [o.placement]] of
    hid : _ -> Just . (.owner) <$> getHero hid
    [] -> pure Nothing

currentKeyControlled :: HasGame m => m Bool
currentKeyControlled = do
  current <- getsGame ((.currentKey) . getMeta . toAttrs . (.scenario))
  held <- controlledKeys
  pure $ any (\o -> Just (toCardCode o) == current) held

setCurrentKey :: ReverseQueue m => EncounterCard -> m ()
setCurrentKey card = push $ ScenarioSpecific "threeTrials.currentKey" (toJSON $ toCardCode card)

-- | One of the set-aside cards with the trait, chosen at random.
randomSetAside :: (HasGame m, MonadRandom m) => Trait -> m (Maybe EncounterCard)
randomSetAside trait = do
  cards <- getsGame (filter (member trait . cdCardTraits . toCardDef) . (.scenarioSetAside) . toAttrs . (.scenario))
  headMay <$> shuffleM cards

-- | The set-aside card with the trait, a Key or a Guardian, that shares a Trait with the card.
setAsideFor :: HasGame m => Trait -> CardDef -> m (Maybe EncounterCard)
setAsideFor trait def = getsGame (find shares . (.scenarioSetAside) . toAttrs . (.scenario))
 where
  traits c = cdCardTraits (toCardDef c)
  shares c = trait `member` traits c && any (\t -> t `member` cdCardTraits def && t `member` traits c) clanTraits

-- | Reveal a set-aside card, adding it to the staging area.
revealSetAside :: ReverseQueue m => EncounterCard -> m ()
revealSetAside = push . ScenarioSpecific "threeTrials.revealSetAside" . toJSON

-- | Reveal a card in the victory display, adding it to the staging area.
revealFromVictoryDisplay :: ReverseQueue m => EncounterCard -> m ()
revealFromVictoryDisplay = push . ScenarioSpecific "threeTrials.revealFromVictoryDisplay" . toJSON

-- | Attach the set-aside Key to the card in play that came from the given card.
attachSetAsideKey :: ReverseQueue m => EncounterCard -> EncounterCard -> m ()
attachSetAsideKey key host = push $ ScenarioSpecific "threeTrials.attachKey" (toJSON (key, host.id))

-- | Shuffle the set-aside card into the bottom 10 cards of the encounter deck.
shuffleIntoBottomTen :: ReverseQueue m => EncounterCard -> m ()
shuffleIntoBottomTen = push . ScenarioSpecific "threeTrials.bottomTen" . toJSON

-- | Each Guardian enemy engages the player the current stage says.
engageGuardians :: ReverseQueue m => m ()
engageGuardians = push $ ScenarioSpecific "threeTrials.engageGuardians" Null

-- | "If unattached, the first player must claim this and attach it to a hero he controls."
keyAbilities :: ObjectiveAttrs -> [Ability]
keyAbilities a = [mkAbility a 1 $ silent $ CardEntersStagingAreaWindow After (toTarget a)]

{- | A Key: claimed as it enters the staging area, discarded to the staging area (as any
objective is), and as a shadow "Shuffle this back into the encounter deck."
-}
runKey :: Message -> ObjectiveAttrs -> QueueT Message GameT ObjectiveAttrs
runKey msg attrs = case msg of
  UseCardAbility _ (isSource attrs -> True) 1 _ -> do
    lead <- getLead
    claimObjective lead attrs
    pure attrs
  ResolveShadowEffect cid eid _ _ | cid == attrs.objectiveCardId -> do
    push $ RemoveShadowCard eid cid
    push $ PutOnTopOfEncounterDeck (MkEncounterCard cid attrs.objectiveCardCode)
    shuffleEncounterDeck
    pure attrs
  _ -> runMessage msg attrs

{- | A trial: "When the players control the current trial's Key objective, [what the
action does] and immediately advance to a stage 2A of the players' choice."
-}
runTrial :: QueueT Message GameT () -> Message -> QuestAttrs -> QueueT Message GameT QuestAttrs
runTrial onDefeat msg attrs = case msg of
  ClaimObjective {} -> do
    checkStage attrs.questId
    runMessage msg attrs
  CheckStage qid | qid == attrs.questId -> do
    whenM currentKeyControlled do
      onDefeat
      completeQuest attrs.questId
    pure attrs
  _ -> runMessage msg attrs

-- | "If <Guardian> is in play or in the victory display", as a nightmare Spirit reads.
guardianAwake :: HasGame m => CardDef -> m Bool
guardianAwake guardian = do
  inPlay <- selectAny (EnemyWithCardCode $ toCardCode guardian)
  won <- getsGame (any ((== toCardCode guardian) . toCardCode) . (.victoryDisplay) . toAttrs . (.scenario))
  pure $ inPlay || won

{- | "Forced: At the beginning of the encounter phase, <Spirit> engages the player who
controls <its Key> or is engaged with <its Guardian>, if able."
-}
spiritEngages :: ReverseQueue m => EnemyId -> CardDef -> CardDef -> m ()
spiritEngages eid spirit guardian = do
  holder <- keyHolder spirit
  guardians <- traverse getEnemy =<< select (EnemyWithCardCode $ toCardCode guardian)
  spiritAt <- placementController . (.placement) <$> getEnemy eid
  for_ (holder <|> headMay (mapMaybe (placementController . (.placement)) guardians)) \pid ->
    unless (spiritAt == Just pid) $ engageEnemy pid eid

-- | An enemy sharing a Boar, Wolf or Raven Trait with any of the cards.
sharingClanTrait :: [CardDef] -> EnemyMatcher
sharingClanTrait defs =
  NotEnemy $ EnemyMatchAll [NotEnemy (EnemyWithTrait t) | t <- clanTraits, any (member t . cdCardTraits) defs]
