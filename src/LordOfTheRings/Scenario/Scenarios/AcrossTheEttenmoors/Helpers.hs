-- | What the cards of Across the Ettenmoors share: the rules of the safe keyword, the
-- side quest the encounter deck is discarded down to, and the damaged characters its
-- enemies count.
module LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers (
  safeIsActive,
  isSafe,
  safeAbilities,
  safeModifiers,
  runSafe,
  discardUntilSideQuest,
  sideQuestsAmong,
  damagedCharactersOf,
  isBlank,
  guardedBy,
  isSafeCard,
  discardUntilSafeFor,
  guardWithSafe,
) where

import LordOfTheRings.Ability
import LordOfTheRings.Card (CardDef (..), CardType (EncounterSideQuestType), EncounterCard, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor (HasModifiersM, modified)
import LordOfTheRings.Entities (entitiesLocations)
import LordOfTheRings.Game.Base (gameEntities, gameScenario)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Id (CardId, LocationId, PlayerId)
import LordOfTheRings.Keyword (Keyword (Safe))
import LordOfTheRings.Location.Types (LocationAttrs)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier (ModifierType (Blank, ImmuneToPlayerCardEffects, NoEngagementChecks))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation, AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Scenario.Types (scenarioEncounterDeck)
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Window

-- | A location with the safe keyword is the active location.
safeIsActive :: HasGame m => m Bool
safeIsActive = selectAny (IsActiveLocation <> LocationWithKeyword Safe)

isSafe :: HasGame m => LocationId -> m Bool
isSafe lid = selectAny (LocationWithId lid <> LocationWithKeyword Safe)

{- | The safe keyword (the scenario's rules), while the location is active: at the end of
the quest phase it is added to the victory display, and the "when revealed" effects of
treachery cards are ignored. The abilities take indexes 101 and 102.
-}
safeAbilities :: LocationAttrs -> [Ability]
safeAbilities a =
  [ability | a.placement == ActiveLocation, ability <- [mkAbility a 101 $ silent $ PhaseEndsWindow When QuestPhase, mkAbility a 102 $ silent $ TreacheryRevealedWindow When]]

{- | An objective-location is immune to player card effects; while a safe location is
active, enemies make no engagement checks and each encounter side quest's text box is
blank.
-}
safeModifiers :: HasModifiersM m => LocationAttrs -> m ()
safeModifiers a = do
  modified a a [ImmuneToPlayerCardEffects]
  when (a.placement == ActiveLocation) do
    modified a GameTarget [NoEngagementChecks]
    sideQuests <- getEncounterSideQuests
    for_ sideQuests \q -> modified a q [Blank]

{- | When the players travel to a safe location, every engaged enemy returns to the staging
area. The safe abilities resolve as 'safeAbilities' reads.
-}
runSafe :: Message -> LocationAttrs -> QueueT Message GameT ()
runSafe msg a = case msg of
  TravelTo lid | isTarget a (LocationTarget lid) -> returnEngagedEnemiesToStaging
  UseCardAbility _ (isSource a -> True) 101 _ -> addToVictoryDisplay a
  UseCardAbility pid (isSource a -> True) 102 ws -> for_ [t | Window _ (Revealed t) <- ws] (cancelWhenRevealed pid)
  _ -> pure ()

-- | Discard cards from the top of the encounter deck until a side quest is discarded, or
-- the deck runs out; the source answers 'DiscardedTopOfEncounterDeck' with them.
discardUntilSideQuest :: (ReverseQueue m, Sourceable a, Targetable a) => a -> m ()
discardUntilSideQuest a = do
  deck <- getsGame (scenarioEncounterDeck . toAttrs . gameScenario)
  discardTopOfEncounterDeck a $ maybe (length deck) (+ 1) (findIndex isSideQuestCard deck)

sideQuestsAmong :: [EncounterCard] -> [EncounterCard]
sideQuestsAmong = filter isSideQuestCard

isSideQuestCard :: EncounterCard -> Bool
isSideQuestCard = (== EncounterSideQuestType) . cdCardType . toCardDef

-- | The damaged characters the player controls.
damagedCharactersOf :: HasGame m => PlayerId -> m [Target]
damagedCharactersOf pid = damagedCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)

-- | Whether the card's text box is blank, as a side quest's is while a safe location is active.
isBlank :: (HasGame m, Targetable a) => a -> m Bool
isBlank a = elem Blank <$> getModifiers a

-- | The locations the encounter card guards.
guardedBy :: HasGame m => CardId -> m [LocationId]
guardedBy cid =
  getsGame (map (.id) . filter ((== AttachedTo (CardIdTarget cid)) . (.placement)) . toList . entitiesLocations . gameEntities)

-- | A location card with the safe keyword.
isSafeCard :: EncounterCard -> Bool
isSafeCard = member Safe . cdKeywords . toCardDef

{- | Nightmare: "discard cards from the top of the encounter deck until a location with the
safe keyword is discarded", for the side quest just revealed: the source reads the cards
back as 'DiscardedTopOfEncounterDeck' with the side quest's card as the target, and calls
'guardWithSafe'.
-}
discardUntilSafeFor :: (ReverseQueue m, Sourceable a) => a -> CardId -> m ()
discardUntilSafeFor a sideQuest = do
  deck <- getsGame (scenarioEncounterDeck . toAttrs . gameScenario)
  discardTopOfEncounterDeckFor a (CardIdTarget sideQuest) $ maybe (length deck) (+ 1) (findIndex isSafeCard deck)

-- | "Attach that location to the newly revealed side quest, as a guarded objective."
guardWithSafe :: ReverseQueue m => CardId -> [EncounterCard] -> m ()
guardWithSafe sideQuest cards = for_ (find isSafeCard cards) \l -> do
  removeFromEncounterDiscard l
  putLocationIntoPlay l (AttachedTo $ CardIdTarget sideQuest)
