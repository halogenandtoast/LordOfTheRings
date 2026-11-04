{- | The game's verbs, for cards to push in the order they read. Each needs a
'ReverseQueue', the queue a card runs its messages with (see
"LordOfTheRings.Classes.RunMessage"); a card can still 'push' a message no helper
covers, in the same order.
-}
module LordOfTheRings.Message.Lifted (
  module LordOfTheRings.Message.Lifted,
  module X,
) where

import LordOfTheRings.Campaign (CampaignLogUpdate)
import LordOfTheRings.Card (Card (..), CardDef, CardType, EncounterCard, HasCardCode (..), IsCard (..), PlayerCard, cdCardTraits, cdCardType, toCardDef)
import LordOfTheRings.Classes.Entity (Entity (toAttrs))
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Frontend (Frontend)
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Effect
import LordOfTheRings.Entities (entitiesLocations, entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities, gamePhase)
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource)
import LordOfTheRings.Helpers.Staging (getLocationQuestPoints)
import LordOfTheRings.Ally.Types (allyCardId)
import LordOfTheRings.Hero.Types (heroCardId)
import LordOfTheRings.I18n (ikey, intVar)
import LordOfTheRings.I18n (textVar)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted.Choose as X
import LordOfTheRings.Message.Lifted.Queue as X
import LordOfTheRings.Modifier
import LordOfTheRings.Outcome
import LordOfTheRings.Player.Types (discardableHand)
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Placement (
  Placement (ActiveLocation, AttachedTo, StagingArea),
  placementController,
 )
import LordOfTheRings.Prelude hiding (count)
import LordOfTheRings.Queue
import LordOfTheRings.Quest.Types (isSideQuest)
import LordOfTheRings.Question (Question (QuestionLabel))
import LordOfTheRings.Source
import LordOfTheRings.Sphere (Sphere)
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window (
  Timing (After, When),
  Window (..),
  WindowType (AllyDiscardedByCardEffect, CharacterRescued, DeclaredAsDefender, DefendedAttack, EnemyAttacking, ShadowEffectTriggered, StagingStepEnds, ThreatRaisedByDoomed, ThreatRaisedByEncounter, ThreatReduced, Traveled, WhenRevealedCanceled),
 )

dealDamage
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
dealDamage source target n = push $ DealDamage (toSource source) (toTarget target) n

healDamage
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
healDamage source target n = push $ HealDamage (toSource source) (toTarget target) n

placeTokens
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Token -> Int -> m ()
placeTokens source target token n = push $ PlaceTokens (toSource source) (toTarget target) token n

placeProgress
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
placeProgress source target = placeTokens source target Progress

placeResources
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
placeResources source target = placeTokens source target Resource

removeTokens
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Token -> Int -> m ()
removeTokens source target token n = push $ RemoveTokens (toSource source) (toTarget target) token n

-- | Progress straight onto the current quest stage, past any active location.
placeProgressOnCurrentQuest :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeProgressOnCurrentQuest source n = traverse_ (\q -> placeProgress source q n) =<< getCurrentQuest

{- | Progress a card places on the current quest: the active location takes it first and
the rest goes to the quest (FAQ 2.02). An active location immune to the card ignores it,
and none of it reaches the quest (FAQ 2.41).
-}
placeProgressOnQuest :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeProgressOnQuest source n =
  select IsActiveLocation >>= \case
    [] -> placeProgressOnCurrentQuest source n
    lid : _ -> do
      immune <- immuneToSource (toSource source) lid
      unless immune do
        l <- getLocation lid
        room <- max 0 . subtract l.progress <$> getLocationQuestPoints lid
        let k = min n room
        when (k > 0) $ placeProgress source lid k
        when (n > k) $ placeProgressOnCurrentQuest source (n - k)

-- | Place time counters on the card, as "place 3 time counters on this stage" reads.
placeTimeCounters
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
placeTimeCounters source target = placeTokens source target TimeCounter

{- | Remove time counters from the card, if able: removing its last opens the window its
Forced answers.
-}
removeTimeCounters
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
removeTimeCounters source target = removeTokens source target TimeCounter

-- | As "add 1 time counter to the current quest" reads.
placeTimeCountersOnCurrentQuest :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeTimeCountersOnCurrentQuest source n = traverse_ (\q -> placeTimeCounters source q n) =<< getCurrentQuest

-- | As "remove 1 time counter from the current quest" reads.
removeTimeCountersFromCurrentQuest :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
removeTimeCountersFromCurrentQuest source n = traverse_ (\q -> removeTimeCounters source q n) =<< getCurrentQuest

removeProgressFromQuest :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
removeProgressFromQuest source n = push $ RemoveProgressFromQuest (toSource source) n

{- | Remove this much progress from the main quest, not a side quest the players quest
against instead, as the Haradrim cycle's "remove 2 progress from the main quest" reads.
-}
removeProgressFromMainQuest :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
removeProgressFromMainQuest source n = do
  quests <- getsGame (filter (not . isSideQuest) . toList . entitiesQuests . gameEntities)
  for_ quests \q -> when (q.progress > 0) $ removeTokens source q Progress (min n q.progress)

-- | Place progress on the main quest, bypassing the active location, as Umbar Square reads.
placeProgressOnMainQuest :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeProgressOnMainQuest source n = traverse_ (\q -> placeTokens source q Progress n) =<< getMainQuests

-- | Remove this much progress from each quest card in play, the main quest and each side
-- quest, as "remove 2 progress from each quest card in play" reads.
removeProgressFromEachQuest :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
removeProgressFromEachQuest source n = do
  quests <- getsGame (toList . entitiesQuests . gameEntities)
  for_ quests \q -> when (q.progress > 0) $ removeTokens source q Progress (min n q.progress)

-- | The quest card becomes the current quest until the end of the phase: a side quest in
-- the staging area, or the main quest, putting aside a side quest chosen before.
setCurrentQuest :: (ReverseQueue m, Targetable target) => target -> m ()
setCurrentQuest = push . SetCurrentQuest . toTarget

{- | The player chooses a quest card in play other than the current quest to be the current
quest until the end of the phase, as Pressing Needs and Centuries of Sorrow read; one that
cannot be the current quest, as Secrets of Rhudaur, is not offered.
-}
chooseNewCurrentQuest :: (ReverseQueue m, Targetable source) => PlayerId -> source -> m ()
chooseNewCurrentQuest pid source = do
  current <- getCurrentQuest
  quests <- filterM (fmap (notElem CannotBeCurrentQuest) . getModifiers) . filter ((/= current) . Just) =<< getQuestCards
  chooseOneM pid do
    questionLabeled "chooseNewCurrentQuest"
    causedBy source
    targets quests setCurrentQuest

raiseThreat :: ReverseQueue m => PlayerId -> Int -> m ()
raiseThreat pid n = push $ RaiseThreat pid n

{- | Raise the player's threat by an encounter or quest card's effect, opening the window
Elfhelm answers.
-}
encounterRaisesThreat :: ReverseQueue m => PlayerId -> Int -> m ()
encounterRaisesThreat pid n = when (n > 0) do
  push $ RaiseThreat pid n
  push $ CheckWindows [Window After $ ThreatRaisedByEncounter pid n]

{- | Each player raises his threat by N from the Doomed keyword, opening his window for
it, as a player card with Doomed N played does; Close Call reads it with its X.
-}
raiseThreatFromDoomed :: ReverseQueue m => Int -> m ()
raiseThreatFromDoomed n = when (n > 0) $ eachPlayer \pid -> do
  raiseThreat pid n
  push $ CheckWindows [Window After $ ThreatRaisedByDoomed pid]

-- | Reduce the player's threat, unless it cannot be, as The Empty Mug forbids.
lowerThreat :: ReverseQueue m => PlayerId -> Int -> m ()
lowerThreat pid n = do
  barred <- elem CannotReduceThreat <$> getModifiers (PlayerTarget pid)
  unless barred $ push $ LowerThreat pid n

{- | The card reduces the player's threat, as 'lowerThreat', opening the window for it if
it does reduce it: The Empty Mug answers a player card's.
-}
lowerThreatBy :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> Int -> m ()
lowerThreatBy source pid n = do
  barred <- elem CannotReduceThreat <$> getModifiers (PlayerTarget pid)
  threat <- (.threat) <$> getPlayer pid
  unless (barred || n <= 0 || threat <= 0) do
    push $ LowerThreat pid n
    push $ CheckWindows [Window After $ ThreatReduced pid (toSource source)]

-- | The player draws N cards by the source's effect, unless player card effects cannot
-- make him draw and it is a player card.
drawCards :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> Int -> m ()
drawCards source pid n = do
  mods <- getModifiers (PlayerTarget pid)
  let barred = CannotDrawCardsFromCardEffects `elem` mods || (CannotDrawCardsFromPlayerCardEffects `elem` mods && isPlayerCardSource (toSource source))
  unless barred $ push $ DrawCards $ Drawing pid n

revealEncounterCard :: ReverseQueue m => m ()
revealEncounterCard = push RevealEncounterCard

-- | The card, taken from the encounter deck, is revealed; an enemy is put into play engaged
-- with the player, as Murzag reads.
revealEncounterCardEngaged :: ReverseQueue m => PlayerId -> EncounterCard -> m ()
revealEncounterCardEngaged pid card = do
  push $ RemoveFromEncounterDeck card
  push $ RevealCardEngaged pid card

revealEncounterCards :: ReverseQueue m => Int -> m ()
revealEncounterCards n = replicateM_ n revealEncounterCard

{- | Search the encounter deck for a matching card, add it to the staging area, and
shuffle the deck.
-}
searchEncounterDeckToStaging :: ReverseQueue m => PlayerId -> CardMatcher -> m ()
searchEncounterDeckToStaging pid matcher = push $ Search pid EncounterDeck matcher ToStaging Shuffle

{- | A card effect discards the card from play. An ally so discarded opens a window once it
is gone, with its traits as it left, as Éothain answers; one destroyed by damage is
discarded by the game, not this.
-}
discard :: (ReverseQueue m, Targetable target) => target -> m ()
discard (toTarget -> t) = do
  push $ Discard t
  case t of
    AllyTarget aid -> do
      present <- selectAny (AllyWithId aid)
      when present do
        def <- toCardDef <$> getAlly aid
        gained <- mapMaybe (\case GainTrait x -> Just x; _ -> Nothing) <$> getModifiers t
        push $ CheckWindows [Window After $ AllyDiscardedByCardEffect t (cdCardTraits def <> setFromList gained)]
    _ -> pure ()

{- | A card effect readies the card, unless it is a character player card effects cannot
ready, as Find Water reads.
-}
ready :: (ReverseQueue m, Targetable target) => target -> m ()
ready (toTarget -> t) = do
  barred <- (CannotBeReadiedByPlayerCards `elem`) . concat <$> sequence [getModifiers GameTarget, getModifiers t]
  let character = case t of
        HeroTarget _ -> True
        AllyTarget _ -> True
        _ -> False
  unless (character && barred) $ push (Ready t)

exhaust :: (ReverseQueue m, Targetable target) => target -> m ()
exhaust = push . Exhaust . toTarget

-- | The character is committed to the quest, as Avalanche! commits those it exhausts.
commitToQuest :: (ReverseQueue m, Targetable target) => target -> m ()
commitToQuest = push . CommitToQuest . toTarget

engageEnemy :: ReverseQueue m => PlayerId -> EnemyId -> m ()
engageEnemy pid eid = push $ EngageEnemy pid eid

gameOver :: ReverseQueue m => Outcome -> m ()
gameOver = push . GameOver

-- | The players win the game.
winGame :: ReverseQueue m => m ()
winGame = gameOver Won

-- | The player is eliminated from the game, as Blocked by Shadow's discard reads.
eliminatePlayer :: ReverseQueue m => PlayerId -> m ()
eliminatePlayer = push . EliminatePlayer

createEffect
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => source -> target -> [ModifierType] -> EffectDuration -> m ()
createEffect source target modifiers duration = push $ CreateEffect $ Effect (toSource source) (toTarget target) modifiers duration

phaseModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> [ModifierType] -> m ()
phaseModifiers source target modifiers = createEffect source target modifiers UntilEndOfPhase

phaseModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
phaseModifier source target = phaseModifiers source target . pure

roundModifiers
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> [ModifierType] -> m ()
roundModifiers source target modifiers = createEffect source target modifiers UntilEndOfRound

roundModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
roundModifier source target = roundModifiers source target . pure

-- | For each player still in the game, in turn order from the first player.
eachPlayer :: ReverseQueue m => (PlayerId -> m ()) -> m ()
eachPlayer f = traverse_ f =<< getRemainingPlayers

{- | The player chooses a player still in the game, the card causing it marked; with one
player left there is no choice.
-}
choosePlayerM
  :: (ReverseQueue m, Targetable source)
  => PlayerId -> source -> (PlayerId -> QueueT Message m ()) -> m ()
choosePlayerM pid source f = do
  players <- getRemainingPlayers
  chooseOrRunOneM pid do
    questionLabeled "choosePlayer"
    causedBy source
    targets players f

{- | Take matching messages off the game's queue, as cancelling an effect does. A card's
own queue holds only what it has pushed, so this reaches past it to the game's.
-}
cancelMatching :: (MonadTrans t, HasQueue Message m) => (Message -> Bool) -> t m ()
cancelMatching = lift . void . popMessagesMatching

{- | Cancel the When Revealed effects of a card just revealed, and with them the lead's
click to resolve them.
-}
cancelWhenRevealed :: (MonadTrans t, HasQueue Message m, HasQueue Message (t m)) => Target -> t m ()
cancelWhenRevealed t = do
  canceled <- lift $ popMessagesMatching \case
    WhenRevealed t' -> t == t'
    AskToResolveWhenRevealed t' _ -> t == t'
    _ -> False
  -- the window after it, as Ruins of Arnor answers
  pushWhen (notNull canceled) $ CheckWindows [Window After $ WhenRevealedCanceled t]

{- | Cancel damage about to be dealt to the target, from the window before it is dealt, as
"before Patrol Leader is dealt damage ... cancel that damage" reads.
-}
cancelDamage :: (MonadTrans t, HasQueue Message m, Targetable target) => target -> t m ()
cancelDamage target = cancelMatching \case
  DoStep 1 (DealDamage _ t _) -> t == toTarget target
  _ -> False

-- | Heal all the damage on a character.
healAllDamage :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> m ()
healAllDamage source target = healDamage source target =<< getDamage (toTarget target)

-- | The player spends resources of a sphere from his heroes' pools.
spendResources :: ReverseQueue m => PlayerId -> Sphere -> Int -> m ()
spendResources pid sphere n = push $ SpendResources pid sphere n

-- | The players as a group spend resources of a sphere, the player picking from whose
-- heroes' pools, as Heed the Dream reads.
spendGroupResources :: ReverseQueue m => PlayerId -> Sphere -> Int -> m ()
spendGroupResources pid sphere n = push $ SpendGroupResources pid sphere n

-- | The players as a group discard resources from their heroes' pools, the player
-- picking from whose, as prowl reads.
discardGroupResources :: ReverseQueue m => PlayerId -> Int -> m ()
discardGroupResources pid n = push $ DiscardGroupResources pid n

removeFromDiscard :: ReverseQueue m => PlayerId -> CardId -> m ()
removeFromDiscard pid cid = push $ RemoveFromDiscard pid cid

placeEnemy :: ReverseQueue m => EnemyId -> Placement -> m ()
placeEnemy eid placement = push $ PlaceEnemy eid placement

-- | "Return all engaged enemies to the staging area."
returnEngagedEnemiesToStaging :: ReverseQueue m => m ()
returnEngagedEnemiesToStaging = do
  engaged <- select (EnemyEngagedWith AnyPlayer)
  for_ engaged \e -> placeEnemy e StagingArea

{- | The player looks at the top card of the encounter deck; with @mayBottom@ they may
then move it to the bottom.
-}
lookAtTopOfEncounterDeck
  :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> Bool -> m ()
lookAtTopOfEncounterDeck pid source mayBottom = push $ LookAtTopOfEncounterDeck pid (toSource source) mayBottom

-- | The player looks at a shadow card dealt to the enemy.
lookAtShadowCard
  :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> EnemyId -> CardId -> m ()
lookAtShadowCard pid source eid cid = push $ LookAtShadowCard pid (toSource source) eid cid

-- | Return a card from the player's discard pile to their hand.
returnFromDiscardToHand :: ReverseQueue m => PlayerId -> CardId -> m ()
returnFromDiscardToHand pid cid = push $ ReturnFromDiscardToHand pid cid

{- | A shadow effect's two values: the first when the attack is undefended (no defender),
the second otherwise.
-}
ifUndefended :: [Target] -> a -> a -> a
ifUndefended defenders whenUndefended otherwise' = if null defenders then whenUndefended else otherwise'

{- | The enemy gets +N attack until its attack resolves, as "attacking enemy gets +1
attack" reads.
-}
enemyGetsAttack :: (ReverseQueue m, Sourceable source) => source -> EnemyId -> Int -> m ()
enemyGetsAttack source eid n = attackModifier source eid (AttackModifier n)

{- | A modifier that lasts until the attack being made resolves, as a shadow effect's
"for this attack" does.
-}
attackModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
attackModifier source target m = createEffect source target [m] UntilEndOfAttack

-- | The player chooses and discards 1 of the cards.
chooseAndDiscard :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> [Target] -> m ()
chooseAndDiscard source pid cards = push $ ChooseAndDiscard (toSource source) pid cards

-- | Deal the enemy 1 more shadow card.
dealShadowCard :: ReverseQueue m => EnemyId -> m ()
dealShadowCard = push . DealShadowCard

-- | The player moves 1 resource from the pool of a hero they control to another hero's.
moveResourceFromHero
  :: (ReverseQueue m, Sourceable source, Targetable source) => PlayerId -> source -> m ()
moveResourceFromHero pid source = do
  from <- select (HeroControlledBy (PlayerWithId pid) <> HeroWithResources)
  chooseOrRunOneM pid do
    questionLabeled "moveResourceFrom"
    causedBy source
    targets from \h -> do
      others <- select (not_ $ HeroWithId h)
      chooseOrRunOneM pid do
        questionLabeled "moveResourceTo"
        causedBy source
        targets others \h' -> do
          removeTokens source h Resource 1
          placeResources source h' 1

-- | Act on the card this one is attached to; nothing when it is not attached.
withHost :: (Applicative m, HasField "placement" a Placement) => a -> (Target -> m ()) -> m ()
withHost a f = case a.placement of
  AttachedTo host -> f host
  _ -> pure ()

addToStaging :: ReverseQueue m => EncounterCard -> m ()
addToStaging = push . AddToStaging

putQuestIntoPlay :: ReverseQueue m => EncounterCard -> m ()
putQuestIntoPlay = push . PutQuestIntoPlay

-- | Turn the stage over to its B side, as a stage that waits to flip does when it may.
flipStage :: (ReverseQueue m, Targetable a) => a -> m ()
flipStage = push . Flip . toTarget

-- | Put a stage into play on its B side, as Hasty Council makes the one chosen the active quest.
putQuestIntoPlayFlipped :: ReverseQueue m => EncounterCard -> m ()
putQuestIntoPlayFlipped card = do
  putQuestIntoPlay card
  flipStage card

placeOrcGuard :: ReverseQueue m => PlayerId -> m ()
placeOrcGuard = push . PlaceOrcGuard

{- | The player card, from its owner's deck, hand or discard pile, is reanimated: a
Reanimated Dead enemy in the staging area (The Dread Realm). Its id is given back for
what the card does to it next, as dealing it a shadow card.
-}
reanimate :: ReverseQueue m => CardId -> m EnemyId
reanimate cid = do
  eid <- getRandom
  push $ Reanimate cid eid
  pure eid

attachTreachery :: (ReverseQueue m, Targetable target) => TreacheryId -> target -> m ()
attachTreachery tid = push . AttachTreachery tid . toTarget

-- | The player chooses and exhausts N of their characters.
chooseAndExhaust :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> Int -> m ()
chooseAndExhaust source pid n = push $ ChooseAndExhaust (toSource source) pid n

-- | The player chooses and exhausts a ready hero they control, if they have one.
chooseAndExhaustHero :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> m ()
chooseAndExhaustHero source = push . ChooseAndExhaustHero (toSource source)

{- | The player searches the top N cards of their deck for matching cards to take into
hand, any number of them or only 1, then shuffles their deck.
-}
searchDeck
  :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> Int -> CardMatcher -> Bool -> m ()
searchDeck pid source n matcher anyNumber = do
  deck <- (.deck) <$> getPlayer pid
  push $ ChooseFromSearch pid (toSource source) (take n deck) matcher anyNumber

{- | The player searches the top N cards of their deck for up to the given number of
matching allies to put into play, each with the modifiers until the end of the round,
then shuffles their deck.
-}
searchDeckIntoPlay
  :: (ReverseQueue m, Sourceable source)
  => PlayerId -> source -> Int -> CardMatcher -> Int -> [ModifierType] -> m ()
searchDeckIntoPlay pid source n matcher count mods = do
  deck <- (.deck) <$> getPlayer pid
  push $ ChooseFromSearchIntoPlay pid (toSource source) (take n deck) matcher count mods

-- | The player resolves their attacks before any enemy attacks this combat phase.
attackBeforeEnemies :: (MonadTrans t, HasQueue Message m) => PlayerId -> t m ()
attackBeforeEnemies pid = lift $ withQueue_ \q ->
  if PlayerAttacks pid `notElem` q
    then q
    else
      let (before, after) = break (\case EnemyAttacks _ -> True; _ -> False) (filter (/= PlayerAttacks pid) q)
       in before <> [PlayerAttacks pid] <> after

discardTopOfDeck :: ReverseQueue m => PlayerId -> Int -> m ()
discardTopOfDeck pid n = push $ DiscardTopOfDeck pid n

shufflePlayerDeck :: ReverseQueue m => PlayerId -> m ()
shufflePlayerDeck = push . ShufflePlayerDeck

-- | A card from out of play goes on top of the player's deck, as Miruvor or the cards
-- underneath Gather Your Courage.
putOnTopOfDeck :: ReverseQueue m => PlayerId -> Card -> m ()
putOnTopOfDeck pid = push . PutOnTopOfDeck pid

removeFromDeck :: ReverseQueue m => PlayerId -> CardId -> m ()
removeFromDeck pid cid = push $ RemoveFromDeck pid cid

-- | A card in the player's discard pile goes to the bottom of their deck.
putDiscardOnBottomOfDeck :: ReverseQueue m => PlayerId -> CardId -> m ()
putDiscardOnBottomOfDeck pid cid = push $ PutDiscardOnBottomOfDeck pid cid

-- | A card in the player's discard pile is shuffled into their deck.
shuffleFromDiscardIntoDeck :: ReverseQueue m => PlayerId -> CardId -> m ()
shuffleFromDiscardIntoDeck pid cid = push $ ShuffleFromDiscardIntoDeck pid cid

-- | The top N cards of the player's discard pile are shuffled into their deck.
shuffleTopOfDiscardIntoDeck :: ReverseQueue m => PlayerId -> Int -> m ()
shuffleTopOfDiscardIntoDeck pid n = push $ ShuffleTopOfDiscardIntoDeck pid n

-- | An ally card in the player's hand is put into play, not played.
putAllyIntoPlayFromHand :: ReverseQueue m => PlayerId -> CardId -> m ()
putAllyIntoPlayFromHand pid = push . PutAllyIntoPlay pid

{- | An ally card, already taken from where it was, goes into play under the player's
control.
-}
putAllyIntoPlayUnder :: ReverseQueue m => PlayerId -> PlayerCard -> m ()
putAllyIntoPlayUnder pid card = push $ PutAllyIntoPlayUnder pid card

{- | The player looks at the top N cards of the encounter deck, reveals 1 and discards
the rest.
-}
revealOneOfTopEncounterCards
  :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> Int -> m ()
revealOneOfTopEncounterCards pid source n = push $ RevealOneOfTopEncounterCards pid (toSource source) n False

{- | As 'revealOneOfTopEncounterCards', the rest going to the bottom of the deck, as Branching
Paths reads.
-}
revealOneOfTopEncounterCardsRestToBottom
  :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> Int -> m ()
revealOneOfTopEncounterCardsRestToBottom pid source n = push $ RevealOneOfTopEncounterCards pid (toSource source) n True

{- | Discard the top N cards of the encounter deck; the card reads them back as
'DiscardedTopOfEncounterDeck' with itself as source and target, as "discard the top card
of the encounter deck. If it is an enemy..." reads.
-}
discardTopOfEncounterDeck :: (ReverseQueue m, Sourceable a, Targetable a) => a -> Int -> m ()
discardTopOfEncounterDeck a = discardTopOfEncounterDeckFor a a

{- | As 'discardTopOfEncounterDeck', read back on behalf of the target, as "each player
discards the top card ... the discarding player" reads with each player.
-}
discardTopOfEncounterDeckFor
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
discardTopOfEncounterDeckFor source target n = push $ DiscardTopOfEncounterDeck (toSource source) (toTarget target) n

{- | Cards are discarded from the top of the encounter deck until one that matches is, as
"discard cards from the top of the encounter deck until a Ship enemy is discarded" reads;
the source reads them back from 'DiscardedTopOfEncounterDeck'.
-}
discardEncounterCardsUntil :: (ReverseQueue m, Sourceable a, Targetable a) => a -> CardMatcher -> m ()
discardEncounterCardsUntil a matcher = do
  deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
  discardTopOfEncounterDeck a $ maybe (length deck) (+ 1) (findIndex (cardMatches matcher) deck)

-- | An encounter card, taken from where it was, becomes the active location, as a setup puts one.
putIntoPlayAsActiveLocation :: ReverseQueue m => EncounterCard -> m ()
putIntoPlayAsActiveLocation = push . PutIntoPlayAsActiveLocation

-- | An enemy card, taken from where it was, comes into play engaged with the player.
putEnemyIntoPlayEngaged :: ReverseQueue m => PlayerId -> EncounterCard -> m ()
putEnemyIntoPlayEngaged pid = push . PutEnemyIntoPlayEngaged pid

removeFromEncounterDeck :: ReverseQueue m => EncounterCard -> m ()
removeFromEncounterDeck = push . RemoveFromEncounterDeck

removeFromEncounterDiscard :: ReverseQueue m => EncounterCard -> m ()
removeFromEncounterDiscard = push . RemoveFromEncounterDiscard

{- | The player pays 1 resource of the sphere or discards the card; with nothing to pay,
the card is discarded.
-}
payOrDiscard :: (ReverseQueue m, Targetable target) => PlayerId -> target -> Sphere -> m ()
payOrDiscard pid card sphere = do
  able <- (> 0) <$> getResourcesOfSphere pid sphere
  if able
    then chooseOneM pid do
      questionLabeled "payOrDiscard"
      causedBy card
      labeled "payResource" $ spendResources pid sphere 1
      labeled "discardIt" $ discard card
    else discard card

-- | Reveal the next encounter card once the card's revelation, its keywords too, has resolved.
revealEncounterCardAfter :: (MonadTrans t, HasQueue Message m) => Target -> t m ()
revealEncounterCardAfter t = lift $ insertAfterMatching [RevealEncounterCard] \case
  WhileInPlay t' _ -> t == t'
  _ -> False

{- | The player does not raise their threat for questing unsuccessfully, from the window
before it is raised, as Ever Onward reads.
-}
doesNotRaiseThreat :: (MonadTrans t, HasQueue Message m) => PlayerId -> t m ()
doesNotRaiseThreat pid = lift $ withQueue_ \q -> case break (\case RaiseThreat p _ -> p == pid; _ -> False) q of
  (before, _ : after) -> before <> after
  _ -> q

-- | Messages to run once the enemy's attack has resolved, as "after this attack" reads.
afterAttack :: (MonadTrans t, HasQueue Message m) => EnemyId -> [Message] -> t m ()
afterAttack eid msgs = lift $ insertAfterMatching msgs \case
  CheckWindows ws -> any (\case Window After (EnemyAttacking eid' _) -> eid == eid'; _ -> False) ws
  _ -> False

{- | "If this attack destroys a character": the shadow's next step, 'DoStep' 1 of its
'ResolveShadowEffect', once the enemy's attack is over, if its damage destroyed one. The
shadow card, built again, resolves it then, and may read what was destroyed with
'attackDestroyed'.
-}
ifThisAttackDestroys :: (MonadTrans t, HasQueue Message m) => EnemyId -> Message -> t m ()
ifThisAttackDestroys eid shadow = afterAttack eid [IfAttackDestroyed eid [DoStep 1 shadow]]

{- | Until the end of the phase, the enemy cannot attack the player it is engaged with now,
as "that enemy cannot attack that player this phase" reads.
-}
cannotAttackEngagedPlayer :: (ReverseQueue m, Sourceable source) => source -> EnemyId -> m ()
cannotAttackEngagedPlayer source eid = do
  engaged <- placementController . (.placement) <$> getEnemy eid
  for_ engaged \pid -> phaseModifier source eid (CannotAttackPlayer pid)

{- | Cancel the shadow effects of the shadow cards whose effects are about to resolve in the
windows, as Hasty Stroke does.
-}
cancelShadowEffects :: (MonadTrans t, HasQueue Message m) => [Window] -> t m ()
cancelShadowEffects windows =
  for_ [cid | Window _ (ShadowEffectTriggered cid _) <- windows] \cid ->
    cancelMatching \case
      ResolveShadowCard cid' _ _ _ -> cid == cid'
      _ -> False

returnToHand :: (ReverseQueue m, Targetable target) => target -> m ()
returnToHand = push . ReturnToHand . toTarget

-- | A player card from out of play goes to its owner's hand, as Lost in the Wilderness
-- returns the cards underneath it.
putInHand :: ReverseQueue m => PlayerCard -> m ()
putInHand = push . PutInHand

addToVictoryDisplay :: (ReverseQueue m, Targetable target) => target -> m ()
addToVictoryDisplay = push . AddToVictoryDisplay . toTarget

removeFromVictoryDisplay :: ReverseQueue m => Card -> m ()
removeFromVictoryDisplay = push . RemoveFromVictoryDisplay

destroy :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> m ()
destroy source = push . Destroy (toSource source) . toTarget

-- | Whether the damage the source just placed on the card destroys it.
checkDefeated :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> m ()
checkDefeated source = push . CheckDefeated (toSource source) . toTarget

shuffleIntoEncounterDeck :: (ReverseQueue m, Targetable target) => target -> m ()
shuffleIntoEncounterDeck = push . ShuffleIntoEncounterDeck . toTarget

returnToTopOfEncounterDeck :: (ReverseQueue m, Targetable target) => target -> m ()
returnToTopOfEncounterDeck = push . ReturnToTopOfEncounterDeck . toTarget

-- | The character leaves the quest, its willpower no longer counted, as "remove 1 questing
-- hero from the quest" reads.
removeFromQuest :: (ReverseQueue m, Targetable target) => target -> m ()
removeFromQuest = push . RemoveFromQuest . toTarget

-- | The encounter card in play goes to the bottom of the encounter deck.
returnToBottomOfEncounterDeck :: (ReverseQueue m, Targetable target) => target -> m ()
returnToBottomOfEncounterDeck = push . ReturnToBottomOfEncounterDeck . toTarget

shuffleEncounterDeck :: ReverseQueue m => m ()
shuffleEncounterDeck = push $ ShuffleDeck EncounterDeck

-- | Set an encounter card aside, out of play.
setAside :: ReverseQueue m => EncounterCard -> m ()
setAside = push . SetAside

addSetAsideToStaging :: ReverseQueue m => CardMatcher -> m ()
addSetAsideToStaging = push . AddSetAsideToStaging

-- | The set-aside cards are shuffled into the encounter deck, as Snowdrifts shuffles in
-- copies of Snowstorm.
shuffleSetAsideIntoEncounterDeck :: ReverseQueue m => [EncounterCard] -> m ()
shuffleSetAsideIntoEncounterDeck = push . ShuffleSetAsideIntoEncounterDeck

-- | Matching cards in the victory display are shuffled into the encounter deck, as The
-- Dimrill Stair reads.
shuffleVictoryDisplayIntoEncounterDeck :: ReverseQueue m => CardMatcher -> m ()
shuffleVictoryDisplayIntoEncounterDeck = push . ShuffleVictoryDisplayIntoEncounterDeck

shuffleEncounterDiscardIntoDeck :: ReverseQueue m => CardMatcher -> m ()
shuffleEncounterDiscardIntoDeck = push . ShuffleEncounterDiscardIntoDeck

{- | The player searches the encounter deck and discard pile for a matching card to add to
the staging area, then shuffles the encounter deck.
-}
searchEncounterDeckAndDiscard
  :: (ReverseQueue m, Targetable source) => PlayerId -> source -> CardMatcher -> m ()
searchEncounterDeckAndDiscard pid source = push . SearchEncounterDeckAndDiscard pid (toTarget source)

-- | As 'searchEncounterDeckAndDiscard', the card found revealed, as Forecastle Deck reads.
searchEncounterDeckAndDiscardToReveal
  :: (ReverseQueue m, Targetable source) => PlayerId -> source -> CardMatcher -> m ()
searchEncounterDeckAndDiscardToReveal pid source = push . SearchEncounterDeckAndDiscardToReveal pid (toTarget source)

-- | As 'searchEncounterDeckAndDiscard', the enemy found put into play engaged with the player.
searchEncounterDeckAndDiscardToEngage
  :: (ReverseQueue m, Targetable source) => PlayerId -> source -> CardMatcher -> m ()
searchEncounterDeckAndDiscardToEngage pid source = push . SearchEncounterDeckAndDiscardToEngage pid (toTarget source)

-- | Claim the objective onto the host, attaching it there.
attachObjective :: (ReverseQueue m, Targetable host) => ObjectiveId -> host -> m ()
attachObjective oid = push . ClaimObjective oid . toTarget

-- | Move an attachment to another host.
moveAttachment :: (ReverseQueue m, Targetable host) => AttachmentId -> host -> m ()
moveAttachment aid = push . AttachCard aid . toTarget

{- | An objective card, taken from where it was, comes into play attached to the hero, as
"have the first player attach Cave Torch to a hero of his choice" reads.
-}
putObjectiveIntoPlayAttached :: (ReverseQueue m, Targetable host) => EncounterCard -> host -> m ()
putObjectiveIntoPlayAttached card = push . PutObjectiveIntoPlayAttached card . toTarget

-- | An objective card, taken from where it was, comes into the staging area guarded by the
-- card, as "attach it to a Mordor enemy" reads of a Clue card.
putObjectiveIntoPlayGuardedBy :: ReverseQueue m => EncounterCard -> CardId -> m ()
putObjectiveIntoPlayGuardedBy card = push . PutObjectiveIntoPlayGuardedBy card

-- | The card leaves play for good: an objective or a location.
removeFromGame :: (ReverseQueue m, Targetable target) => target -> m ()
removeFromGame = push . RemoveFromGame . toTarget

-- | The character is shuffled into its owner's deck, as Lost and Alone does with a hero.
shuffleIntoOwnersDeck :: (ReverseQueue m, Targetable target) => target -> m ()
shuffleIntoOwnersDeck = push . ShuffleIntoOwnersDeck . toTarget

-- | The ally leaves play for the bottom of its owner's deck, as Curious Brandybuck.
putOnBottomOfOwnersDeck :: (ReverseQueue m, Targetable target) => target -> m ()
putOnBottomOfOwnersDeck = push . PutOnBottomOfOwnersDeck . toTarget

putObjectiveAllyIntoPlay :: ReverseQueue m => ObjectiveId -> PlayerId -> m ()
putObjectiveAllyIntoPlay oid = push . PutObjectiveAllyIntoPlay oid

putHeroIntoPlay :: ReverseQueue m => PlayerId -> PlayerCard -> m ()
putHeroIntoPlay pid = push . PutHeroIntoPlay pid

-- | A treachery dealt as a shadow card comes into play attached to the host.
attachShadowTreachery :: (ReverseQueue m, Targetable host) => EnemyId -> CardId -> host -> m ()
attachShadowTreachery eid cid = push . PutShadowTreacheryIntoPlay eid cid . AttachedTo . toTarget

placeLocation :: ReverseQueue m => LocationId -> Placement -> m ()
placeLocation lid = push . PlaceLocation lid

-- | Make the location the active location, without traveling there.
makeActive :: ReverseQueue m => LocationId -> m ()
makeActive lid = placeLocation lid ActiveLocation

{- | Travel to the location without resolving its Travel effect: it becomes active, and
the players have traveled there.
-}
travelWithoutCost :: ReverseQueue m => LocationId -> m ()
travelWithoutCost lid = do
  makeActive lid
  push $ CheckWindows [Window After (Traveled lid)]

-- | The quest deck goes back to the stage, followed by the stages given.
resetToStage :: ReverseQueue m => EncounterCard -> [EncounterCard] -> m ()
resetToStage card = push . ResetToStage card

-- | Reveal the next stage of the quest deck, as a stage that leaves play without being
-- defeated calls for.
advanceQuest :: ReverseQueue m => m ()
advanceQuest = push AdvanceQuest

-- | Shuffle the quest card, in play or in the victory display, into the quest deck.
shuffleIntoQuestDeck :: (ReverseQueue m, IsCard a) => a -> m ()
shuffleIntoQuestDeck a = case toCard a of
  EncounterCard c -> push $ ShuffleIntoQuestDeck c
  PlayerCard _ -> pure ()

-- | Put the quest card, wherever it is, on the bottom of the quest deck, as bypassing a stage does.
putOnBottomOfQuestDeck :: (ReverseQueue m, IsCard a) => a -> m ()
putOnBottomOfQuestDeck a = case toCard a of
  EncounterCard c -> push $ PutOnBottomOfQuestDeck c
  PlayerCard _ -> pure ()

-- | The stage leaves the quest deck, if it is still there, as exploring The Banks removes
-- stage 2.
removeFromQuestDeck :: ReverseQueue m => CardDef -> m ()
removeFromQuestDeck def = do
  deck <- getsGame ((.questDeck) . toAttrs . (.scenario))
  traverse_ (push . RemoveFromQuestDeck) $ filter ((== toCardCode def) . toCardCode) deck

checkStage :: ReverseQueue m => QuestId -> m ()
checkStage = push . CheckStage

{- | The player puts the cards, which are in their deck, back on top of it in the order
they choose: each goes on top as it is chosen, so the last one chosen ends on top.
-}
returnToTopOfDeckInAnyOrder
  :: (ReverseQueue m, Targetable source) => PlayerId -> source -> [Card] -> m ()
returnToTopOfDeckInAnyOrder pid source cards = chooseOneAtATimeM pid do
  questionLabeled "putOnTopOfDeck"
  causedBy source
  showing cards
  targets cards \c -> push $ MoveToTopOfDeck pid c.id

-- | The player switches a card in hand with one in their deck.
swapHandWithDeck :: ReverseQueue m => PlayerId -> Card -> Card -> m ()
swapHandWithDeck pid fromHand fromDeck = push $ SwapHandWithDeck pid fromHand.id fromDeck.id

-- | Run the message again, as a later step of itself.
doStep :: ReverseQueue m => Int -> Message -> m ()
doStep n = push . DoStep n

{- | Until the end of the phase, damage from the enemy's undefended attacks goes to the
character.
-}
undefendedDamageTo
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> EnemyId -> target -> m ()
undefendedDamageTo source eid = phaseModifier source eid . UndefendedDamageTo . toTarget

{- | The card leaves the encounter discard pile and is revealed, as West Gate's search: its
keywords and windows apply as for any reveal.
-}
revealFromEncounterDiscard :: ReverseQueue m => EncounterCard -> m ()
revealFromEncounterDiscard card = do
  removeFromEncounterDiscard card
  push $ RevealCard card

{- | Only the card's When Revealed effect resolves, as Lieutenant of Mordor's: no keyword,
window or cancel touches it. The caller first takes the card from where it was.
-}
resolveWhenRevealedOf :: ReverseQueue m => EncounterCard -> m ()
resolveWhenRevealedOf = push . ResolveWhenRevealedOf

-- | The card leaves the encounter discard pile, and only its When Revealed effect resolves.
resolveWhenRevealedFromDiscard :: ReverseQueue m => EncounterCard -> m ()
resolveWhenRevealedFromDiscard card = do
  removeFromEncounterDiscard card
  resolveWhenRevealedOf card

-- | After its attack resolves, the enemy returns to the staging area.
returnToStagingAfterAttack :: (MonadTrans t, HasQueue Message m) => EnemyId -> t m ()
returnToStagingAfterAttack eid = afterAttack eid [PlaceEnemy eid StagingArea]

-- | The defending character is removed from the attack, which is now undefended.
makeUndefended
  :: (MonadTrans t, HasQueue Message m, ReverseQueue (t m)) => PlayerId -> EnemyId -> t m ()
makeUndefended pid eid = do
  cancelMatching \case
    AttackDamage eid' _ -> eid == eid'
    DefendedByMany _ eid' _ -> eid == eid'
    _ -> False
  push $ UndefendedDamage pid eid

discardFromHand :: ReverseQueue m => PlayerId -> Card -> m ()
discardFromHand pid card = push $ DiscardFromHand pid card.id

{- | The player makes an escape test against the given number of encounter cards,
counting the stat; what follows a failure and a pass are actions of their own.
-}
escapeTest
  :: (ReverseQueue m, Sourceable source)
  => PlayerId -> source -> Int -> EscapeStat -> QueueT Message m () -> QueueT Message m () -> m ()
escapeTest pid source n stat onFail onPass = do
  failed <- capture onFail
  passed <- capture onPass
  push $ MakeEscapeTest $ EscapeTest pid (toSource source) n stat [] failed passed []

{- | The party makes an escape test: each player still in the game, from the first player,
commits their characters in turn.
-}
partyEscapeTest
  :: (ReverseQueue m, Sourceable source)
  => source -> Int -> EscapeStat -> QueueT Message m () -> QueueT Message m () -> m ()
partyEscapeTest source n stat onFail onPass = do
  failed <- capture onFail
  passed <- capture onPass
  players <- getRemainingPlayers
  for_ (uncons players) \(pid, rest) -> push $ MakeEscapeTest $ EscapeTest pid (toSource source) n stat [] failed passed rest

placeObjective :: ReverseQueue m => ObjectiveId -> Placement -> m ()
placeObjective oid = push . PlaceObjective oid

-- | The card guards the objective, as an encounter card attached to it does.
guardObjectiveWith :: ReverseQueue m => ObjectiveId -> CardId -> m ()
guardObjectiveWith oid = push . SetObjectiveGuard oid

-- | Resolve the objective's Guarded keyword: the next encounter card guards it.
resolveGuarded :: ReverseQueue m => ObjectiveId -> m ()
resolveGuarded = push . GuardObjective

-- | The card goes facedown under the ally, or the Nameless enemy.
attachFacedown :: (ReverseQueue m, Targetable target) => target -> PlayerCard -> m ()
attachFacedown target = push . AttachFacedown (toTarget target)

-- | Discard a shadow card dealt to the enemy, unresolved.
discardShadowCard :: ReverseQueue m => EnemyId -> EncounterCard -> m ()
discardShadowCard eid card = do
  push $ RemoveShadowCard eid card.id
  push $ AddToEncounterDiscard [card]

-- | The encounter card, already taken from where it was, goes on top of the encounter deck.
putOnTopOfEncounterDeck :: ReverseQueue m => EncounterCard -> m ()
putOnTopOfEncounterDeck = push . PutOnTopOfEncounterDeck

-- | The encounter card leaves the discard pile for the top of the encounter deck.
moveToTopOfEncounterDeck :: ReverseQueue m => EncounterCard -> m ()
moveToTopOfEncounterDeck card = do
  push $ RemoveFromEncounterDiscard card
  push $ PutOnTopOfEncounterDeck card

-- | The shadow card, a location, becomes the active location.
putShadowLocationIntoPlay :: ReverseQueue m => EnemyId -> CardId -> m ()
putShadowLocationIntoPlay eid = push . PutShadowLocationIntoPlay eid

-- | Add the shadow card dealt to the enemy to the staging area, as "Shadow: Add Goblin
-- Swordsman to the staging area" reads.
putShadowEnemyIntoStaging :: ReverseQueue m => EnemyId -> CardId -> m ()
putShadowEnemyIntoStaging eid = push . PutShadowEnemyIntoStaging eid

{- | The enemy's shadow cards not yet resolved turn face up and resolve, as shadow cards
dealt during an attack do: the attack's defenders, none when undefended.
-}
resolveShadowCards :: ReverseQueue m => EnemyId -> PlayerId -> [Target] -> m ()
resolveShadowCards eid pid = push . ResolveShadow eid pid

{- | Deal the attacking enemy more shadow cards as its shadow effects resolve, as "deal it
another shadow card" reads: the new ones turn face up and resolve too, with the rest
still face down, which resolve only this once.
-}
dealShadowCardsDuringAttack
  :: (MonadTrans t, HasQueue Message m, ReverseQueue (t m)) => EnemyId -> PlayerId -> [Target] -> Int -> t m ()
dealShadowCardsDuringAttack eid pid defenders n = do
  replicateM_ n $ dealShadowCard eid
  resolveNewShadowCards eid pid defenders

{- | As 'dealShadowCardsDuringAttack', the given cards from the encounter deck, as Flame of
Udûn deals them there rather than from the Balrog deck.
-}
dealShadowCardsFromEncounterDeckDuringAttack
  :: (MonadTrans t, HasQueue Message m, ReverseQueue (t m)) => EnemyId -> PlayerId -> [Target] -> [EncounterCard] -> t m ()
dealShadowCardsFromEncounterDeckDuringAttack eid pid defenders cards = do
  for_ cards \c -> push (RemoveFromEncounterDeck c) >> push (AttachShadowCard eid c)
  resolveNewShadowCards eid pid defenders

-- | The attacking enemy's shadow cards resolve again, the new ones with them.
resolveNewShadowCards
  :: (MonadTrans t, HasQueue Message m, ReverseQueue (t m)) => EnemyId -> PlayerId -> [Target] -> t m ()
resolveNewShadowCards eid pid defenders = do
  cancelMatching \case
    RevealShadowCard eid' _ -> eid' == eid
    ResolveShadowCard _ eid' _ _ -> eid' == eid
    CheckWindows (Window When (ShadowEffectTriggered _ eid') : _) -> eid' == eid
    _ -> False
  resolveShadowCards eid pid defenders

-- | Deal the attacking enemy a card from the encounter discard pile face up and resolve its
-- shadow effect for this attack, as Backtrack! reads.
dealAndResolveShadowCard :: ReverseQueue m => EnemyId -> PlayerId -> [Target] -> EncounterCard -> m ()
dealAndResolveShadowCard eid pid defenders card = do
  removeFromEncounterDiscard card
  push $ AttachShadowCard eid card
  push $ RevealShadowCard eid card.id
  push $ ResolveShadowCard card.id eid pid defenders

-- | Put the shadow card dealt to the enemy into play, engaged with the player.
putShadowEnemyIntoPlay :: ReverseQueue m => EnemyId -> CardId -> PlayerId -> m ()
putShadowEnemyIntoPlay eid cid = push . PutShadowEnemyIntoPlay eid cid

{- | The player returns a matching ally they control to their hand, as "return a Silvan
ally you control to your hand" pays for a card; see 'controlsAlly'.
-}
returnAllyToHand :: (ReverseQueue m, Targetable source) => PlayerId -> source -> AllyMatcher -> m ()
returnAllyToHand pid source matcher = do
  allies <- select (AllyControlledBy (PlayerWithId pid) <> matcher)
  chooseOrRunOneM pid do
    questionLabeled "returnAllyToHand"
    causedBy source
    targets allies returnToHand

{- | Play a card from the player's hand without paying its cost: it still counts as played
from hand.
-}
playFromHandAtNoCost :: ReverseQueue m => PlayerId -> CardId -> m ()
playFromHandAtNoCost pid cid = push $ PlayFromHandAtNoCost pid cid

{- | The character attacks the enemy, wherever it is, as an effect declares it an
attacker; its attack raised by the bonus for that attack alone.
-}
attackEnemyWith
  :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> Target -> EnemyId -> Int -> m ()
attackEnemyWith pid source attacker eid bonus = do
  push $ AttackWith attacker
  when (bonus /= 0) $ createEffect source attacker [AttackModifier bonus] UntilEndOfAttack
  push $ ResolveAttack pid eid

{- | The player attaches a card from their hand to the host without playing it, as "attach
a Weapon or Armour attachment from his hand" reads; 'cardHosts' gives where it may go.
-}
attachFromHand :: (ReverseQueue m, Targetable host) => PlayerId -> Card -> host -> m ()
attachFromHand pid card = push . PutAttachmentIntoPlayFromHand pid card.id . toTarget

{- | The player attaches a card from their discard pile to the host without paying for it,
as Master Ironsmith reads; 'cardHosts' gives where it may go.
-}
attachFromDiscard :: (ReverseQueue m, Targetable host) => PlayerId -> PlayerCard -> host -> m ()
attachFromDiscard pid card host = do
  removeFromDiscard pid card.id
  push $ PlayAttachmentOn pid card (toTarget host)

-- | The player chooses and discards N cards from their hand.
chooseAndDiscardFromHand :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> Int -> m ()
chooseAndDiscardFromHand pid source n = push $ ChooseAndDiscardFromHand pid (toSource source) n

{- | Each player, in turn, may discard any number of cards from his hand; the source reads
the cards back as 'EachDiscardAnyFromHand' with no player left to ask, as Doors of Durin
does.
-}
eachPlayerMayDiscardFromHand :: (ReverseQueue m, Sourceable source) => source -> m ()
eachPlayerMayDiscardFromHand source = do
  players <- getRemainingPlayers
  push $ EachDiscardAnyFromHand (toSource source) players []

-- | The player discards N cards at random from their hand.
discardRandomFromHand :: ReverseQueue m => PlayerId -> Int -> m ()
discardRandomFromHand pid n = push $ DiscardRandomFromHand pid n

-- | The player discards 1 resource from one of their heroes' resource pools, if able.
discardResourceFromAHero :: (ReverseQueue m, Sourceable source, Targetable source) => source -> PlayerId -> m ()
discardResourceFromAHero source pid = do
  heroes <- select (HeroControlledBy (PlayerWithId pid) <> HeroWithResources)
  chooseOrRunOneM pid do
    questionLabeled "discardResourceFromAHero"
    causedBy source
    targets heroes \h -> removeTokens source h Resource 1

{- | The enemy makes an immediate attack against the player, dealt a shadow card for it.
Outside the combat phase its shadow cards are discarded once the attack resolves.
-}
immediateAttack :: ReverseQueue m => PlayerId -> EnemyId -> m ()
immediateAttack pid eid = do
  push $ DealShadowCard eid
  push $ EnemyAttack pid eid
  phase <- getsGame gamePhase
  when (phase /= CombatPhase) $ push $ DiscardShadowCards eid

{- | The enemy engages the player with the highest threat and makes an immediate attack,
the first player choosing among players tied for it, as Prowling Orc's Scour and Jailor
Gornákh read.
-}
engageHighestThreatAndAttack :: (ReverseQueue m, Targetable source) => source -> EnemyId -> m ()
engageHighestThreatAndAttack source eid = withHighestThreatPlayer source \pid -> do
  engageEnemy pid eid
  immediateAttack pid eid

-- | What the player with the highest threat is to do, the first player choosing among
-- players tied for it.
withHighestThreatPlayer :: (ReverseQueue m, Targetable source) => source -> (PlayerId -> QueueT Message m ()) -> m ()
withHighestThreatPlayer source f = do
  players <- traverse (\p -> (p,) <$> getThreat p) =<< getRemainingPlayers
  let highest = foldr (max . snd) 0 players
  lead <- getLead
  chooseOrRunOneM lead do
    questionLabeled "engageHighestThreat"
    causedBy source
    targets [p | (p, threat) <- players, threat == highest] f

-- | The hero leaves play, set aside out of play.
setAsideHero :: ReverseQueue m => HeroId -> m ()
setAsideHero = push . SetAsideHero

-- | The player divides the damage among characters they control, as "that player must deal
-- X damage divided among characters he controls" reads; it is dealt once all is assigned.
dealDamageDivided :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> Int -> m ()
dealDamageDivided source pid n = push $ DealDamageDivided (toSource source) pid n []

-- | The enemy makes an additional attack against the player once this attack resolves, as
-- "attacking enemy makes an additional attack after this one" reads.
additionalAttack
  :: (HasGame m, MonadRandom m, Frontend m, HasQueue Message m) => PlayerId -> EnemyId -> QueueT Message m ()
additionalAttack pid eid = do
  again <- capture $ immediateAttack pid eid
  afterAttack eid again

-- | The player divides the damage among the characters given, as "assign X damage among
-- characters committed to the quest" reads; it is dealt once all is assigned.
dealDamageDividedAmong
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> PlayerId -> [target] -> Int -> m ()
dealDamageDividedAmong source pid among n = push $ DealDamageDividedAmong (toSource source) pid (map toTarget among) n []

-- | The player deals the damage to 1 hero they control, as "each player deals 1 damage to a
-- hero he controls" reads.
dealDamageToOneHero :: (ReverseQueue m, Sourceable source, Targetable source) => source -> PlayerId -> Int -> m ()
dealDamageToOneHero source pid n = do
  heroes <- select (HeroControlledBy $ PlayerWithId pid)
  chooseOrRunOneM pid do
    questionLabeledWith "dealDamageToOneHero" [intVar "n" n]
    causedBy source
    targets heroes \h -> dealDamage source h n

-- | The matching set-aside cards are revealed, as "reveal Orc Ambush and add it to the
-- staging area" reads.
revealSetAsideMatching :: ReverseQueue m => CardMatcher -> m ()
revealSetAsideMatching matcher = do
  setAside' <- getsGame ((.scenarioSetAside) . toAttrs . (.scenario))
  traverse_ (push . RevealCard) $ filter (cardMatches matcher) setAside'

{- | Take the first matching card from the encounter deck, discard pile or victory display,
then shuffle the encounter deck, as "search the encounter deck, discard pile and victory
display for Rescue Iârion" reads: what is found is handed on, to be revealed or staged.
-}
searchEncounterCardsAndVictoryDisplay :: ReverseQueue m => CardMatcher -> (EncounterCard -> m ()) -> m ()
searchEncounterCardsAndVictoryDisplay matcher f = do
  s <- getsGame (toAttrs . (.scenario))
  let found = find (cardMatches matcher)
  case (found s.encounterDeck, found s.encounterDiscard, found [c | EncounterCard c <- s.victoryDisplay]) of
    (Just c, _, _) -> removeFromEncounterDeck c >> f c
    (_, Just c, _) -> removeFromEncounterDiscard c >> f c
    (_, _, Just c) -> removeFromVictoryDisplay (EncounterCard c) >> f c
    _ -> pure ()
  shuffleEncounterDeck

-- | As 'searchEncounterCardsAndVictoryDisplay', the card found revealed.
searchAndRevealEncounterCard :: ReverseQueue m => CardMatcher -> m ()
searchAndRevealEncounterCard matcher = searchEncounterCardsAndVictoryDisplay matcher (push . RevealCard)

{- | End the current phase, as "End the current phase" reads: what is left of it on the
game's queue is dropped, up to its end. A log block it opened still closes.
-}
endCurrentPhase :: (MonadTrans t, HasQueue Message m, HasGame m) => t m ()
endCurrentPhase = do
  phase <- lift $ getsGame gamePhase
  lift $ withQueue_ \q -> case break (== EndPhase phase) q of
    (before, after@(_ : _)) -> filter isClose before <> after
    _ -> q
 where
  isClose = \case
    CloseLogBlock _ -> True
    _ -> False

-- | The player deals the damage to 1 character they control, as "each player must deal 3
-- damage to 1 character he controls" reads.
dealDamageToOneCharacter :: (ReverseQueue m, Sourceable source, Targetable source) => source -> PlayerId -> Int -> m ()
dealDamageToOneCharacter source pid n = do
  characters <- charactersControlledBy pid
  chooseOneM pid do
    questionLabeledWith "dealDamageToOneCharacter" [intVar "n" n]
    causedBy source
    targets characters \c -> dealDamage source c n

-- | Remove all progress tokens from play: from the quest and every location, but those
-- immune to card effects, which keep theirs as they take it.
removeAllProgress :: (ReverseQueue m, Sourceable source) => source -> m ()
removeAllProgress source = do
  quests <- getsGame (toList . entitiesQuests . gameEntities)
  for_ quests \q -> when (q.progress > 0) $ removeTokens source q Progress q.progress
  locations <- getsGame (toList . entitiesLocations . gameEntities)
  for_ locations \l -> when (l.progress > 0) $ removeTokens source l Progress l.progress

-- | The player discards every card in their hand.
discardHand :: ReverseQueue m => PlayerId -> m ()
discardHand pid = traverse_ (discardFromHand pid) . discardableHand =<< getPlayer pid

-- | The card leaves the encounter discard pile for the staging area, as "return the top 2
-- Goblin enemies in the encounter discard pile to the staging area" reads.
addToStagingFromDiscard :: ReverseQueue m => EncounterCard -> m ()
addToStagingFromDiscard card = do
  removeFromEncounterDiscard card
  addToStaging card

-- | Another player takes control of the ally.
takeControl :: ReverseQueue m => AllyId -> PlayerId -> m ()
takeControl aid pid = push $ TakeControl aid pid

-- | The ally card in the player's hand is put into play, exhausted and committed to the
-- quest.
putIntoPlayCommitted :: ReverseQueue m => PlayerId -> CardId -> m ()
putIntoPlayCommitted pid cid = do
  push $ PutAllyIntoPlay pid cid
  push $ CommitToQuest (CardIdTarget cid)

{- | Cancel the effects of an event being played, as Arkham's cancelEvent: its
'PlayEvent' is taken off the game's queue, and the card goes to its owner's discard pile
as a played event does.
-}
cancelEvent :: (MonadTrans t, HasQueue Message m, ReverseQueue (t m)) => CardId -> t m ()
cancelEvent cid = do
  cancelled <- lift $ popMessagesMatching \case
    PlayEvent _ card _ -> card.id == cid
    _ -> False
  for_ [card | PlayEvent _ card _ <- cancelled] (push . PutInDiscard)

-- | The stage is defeated, and the players advance to the next, as "if Murzag is defeated,
-- advance to the next stage" reads.
completeQuest :: ReverseQueue m => QuestId -> m ()
completeQuest = push . CompleteQuest

{- | End the quest phase without resolving the quest, as False Lead reads: the rest of the
staging step, the players' actions and the quest's resolution are dropped. Nothing
happens outside the quest phase.
-}
endQuestPhaseWithoutResolving :: (MonadTrans t, HasQueue Message m) => t m ()
endQuestPhaseWithoutResolving = lift $ withQueue_ \q ->
  case break (== EndPhase QuestPhase) q of
    (before, after@(_ : _)) -> filter (not . questStep) before <> after
    _ -> q
 where
  questStep = \case
    RevealEncounterCard -> True
    CheckWindows [Window When StagingStepEnds] -> True
    ActionWindow _ -> True
    QuestResolution -> True
    _ -> False

-- | A location card, taken from where it was, comes into play placed there, as Street
-- Fighting puts one under the first player's control.
putLocationIntoPlay :: ReverseQueue m => EncounterCard -> Placement -> m ()
putLocationIntoPlay card = push . PutLocationIntoPlay card

-- | The action's messages, unless the card has left play by the time they run.
whileInPlay :: (ReverseQueue m, Targetable target) => target -> QueueT Message m () -> m ()
whileInPlay target action = push . WhileInPlay (toTarget target) =<< capture action

{- | The character, a hero or an ally, becomes a captive guarded by the encounter card: a
guarded objective under no player's control, out of every matcher's reach, until the card
leaves play and it is rescued, exhausted, as "attach that ally to Prison Guard" reads. What
it had on it stays: discard its tokens and attachments first where the card says to.
-}
guardCharacterWith :: (ReverseQueue m, Targetable character, IsCard captor) => character -> captor -> m ()
guardCharacterWith c captor = push $ GuardCharacter (toTarget c) (toCard captor).id

{- | A card out of play becomes a captive guarded by the encounter card, as Torture Room's
"discards cards from the top of his deck until he discards an ally. Attach that ally to
Torture Room" reads: an ally card is taken from its owner's discard pile, an objective
ally comes under the first player. Rescued, it is in play as 'guardCharacterWith's are.
-}
guardCardWith :: (ReverseQueue m, IsCard captor) => Card -> captor -> m ()
guardCardWith card captor = push $ GuardCard card (toCard captor).id

-- | The guarded character is rescued now, as the card guarding it would leave play.
rescueCharacter :: (ReverseQueue m, Targetable character) => character -> m ()
rescueCharacter (toTarget -> c) = pushAll [RescueCharacter c, CheckWindows [Window After $ CharacterRescued c]]

-- | Place the ally elsewhere, as Captured! puts Faramir under itself.
placeAlly :: ReverseQueue m => AllyId -> Placement -> m ()
placeAlly aid = push . PlaceAlly aid

-- | The shadow card just dealt to the enemy is dealt face up, as Silver Lamp reads.
turnShadowCardFaceUp :: ReverseQueue m => EnemyId -> CardId -> m ()
turnShadowCardFaceUp eid = push . TurnShadowCardFaceUp eid

{- | "Then, if your threat is 20 or less and this is the first time you played a copy of
it this round, return this card to your hand instead of discarding it", as Swift and
Silent reads: the event, already in its owner's discard pile, goes back to his hand.
-}
returnIfFirstCopyThisRound :: ReverseQueue m => PlayerId -> PlayerCard -> m ()
returnIfFirstCopyThisRound pid card = do
  played <- elem (PlayedThisRound $ toCardCode card) <$> getModifiers (PlayerTarget pid)
  threat <- (.threat) <$> getPlayer pid
  roundModifier (CardIdSource card.id) (PlayerTarget pid) (PlayedThisRound $ toCardCode card)
  when (not played && threat <= 20) $ returnFromDiscardToHand pid card.id

{- | The player looks at the top card of the encounter deck and, if it is of the type, may
discard it, as Ithilien Lookout reads for an enemy.
-}
lookAtTopOfEncounterDeckMayDiscard
  :: (ReverseQueue m, Sourceable source, Targetable source) => PlayerId -> source -> CardType -> m ()
lookAtTopOfEncounterDeckMayDiscard pid source cType =
  getTopOfEncounterDeck >>= traverse_ \top -> chooseOneM pid do
    questionLabeled "topOfEncounterDeck"
    causedBy source
    showing [EncounterCard top]
    when (cdCardType (toCardDef top) == cType) $ labeled "discardIt" $ discardTopOfEncounterDeck source 1
    labeled "leaveOnTop" (pure ())

-- | The treachery leaves play for the player's hand, where it stays, as Ill Tidings reads.
drawTreacheryIntoHand :: ReverseQueue m => PlayerId -> TreacheryId -> m ()
drawTreacheryIntoHand pid = push . DrawTreacheryIntoHand pid
-- | The player shuffles his discard pile into his deck.
shuffleDiscardIntoDeck :: ReverseQueue m => PlayerId -> m ()
shuffleDiscardIntoDeck = push . ShuffleDiscardIntoDeck

-- | The players make engagement checks, from the first player, as in the encounter phase.
makeEngagementChecks :: ReverseQueue m => m ()
makeEngagementChecks = getLead >>= \lead -> push (EngagementCheck lead 0)
{- | The player card behind the source leaves play, or its owner's discard pile, for good,
as "remove that card from the game" reads of the card behind an effect.
-}
removeSourceCardFromGame :: ReverseQueue m => Source -> m ()
removeSourceCardFromGame = \case
  AbilitySource s _ -> removeSourceCardFromGame s
  AllySource aid -> do
    a <- getAlly aid
    discard aid
    removeFromDiscard a.owner (allyCardId $ toAttrs a)
  HeroSource hid -> do
    h <- getHero hid
    discard hid
    removeFromDiscard h.owner (heroCardId $ toAttrs h)
  AttachmentSource aid ->
    getsGame (lookup aid . (.entities.attachments)) >>= traverse_ \a -> do
      discard aid
      removeFromDiscard (toAttrs a).owner (toAttrs a).cardId
  -- an event, in its owner's discard pile once played
  CardIdSource cid -> eachPlayer \p -> removeFromDiscard p cid
  _ -> pure ()
-- | The cards go facedown underneath the target, out of play.
placeUnderneath :: (ReverseQueue m, Targetable target) => target -> [Card] -> m ()
placeUnderneath target = push . PlaceUnderneath (toTarget target)

{- | Place damage tokens on the card, as encounter cards place them on a location, then
check whether that defeats it.
-}
placeDamage
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
placeDamage source target n = do
  placeTokens source target Damage n
  checkDefeated source target
-- | Advance to the stage, taken from the quest deck, as a stage that picks the next at
-- random does.
advanceToQuest :: ReverseQueue m => EncounterCard -> m ()
advanceToQuest = push . AdvanceToQuest

-- | A card from the player's deck goes to their hand, added rather than drawn, as Hunter
-- of Lamedon adds it.
takeFromDeck :: ReverseQueue m => PlayerId -> Card -> m ()
takeFromDeck pid card = push $ TakeFromDeck pid card.id

{- | An enemy that engages the player during their Resolve Enemy Attacks step attacks in
that step too, dealt no shadow card (ruling), as Murzag's revealed enemy: it joins the
enemies still to attack, or attacks once they have.
-}
attacksThisStep :: (MonadTrans t, HasQueue Message m) => PlayerId -> EnemyId -> t m ()
attacksThisStep pid eid = lift $ withQueue_ \q ->
  if PlayerAttacks pid `notElem` q || EnemyAttacks pid `elem` q
    then q
    else case break (\case ChooseEnemyAttack p _ -> p == pid; _ -> False) q of
      (before, ChooseEnemyAttack p cs : after) -> before <> (ChooseEnemyAttack p (cs <> [eid]) : after)
      _ ->
        let (before, after) = break (\case EnemyAttacks _ -> True; PlayerAttacks _ -> True; _ -> False) q
         in before <> [ChooseEnemyAttack pid [eid]] <> after

{- | Resolve the enemy's attack against another target, as Small Target turns it on another
enemy: its damage step deals that target the damage, reckoned then, and the enemy is no
longer attacking the player (FAQ 3.072), so no window opens after it attacks him.
-}
redirectAttack :: (ReverseQueue m, Targetable target) => EnemyId -> target -> m ()
redirectAttack eid = push . RedirectAttack eid . toTarget

{- | "Create your own staging area": the players split into areas of their own, one for
each group, the first player's first. See 'LordOfTheRings.Game.Base.Separation'.
-}
separatePlayers :: ReverseQueue m => [[PlayerId]] -> m ()
separatePlayers = push . SeparatePlayers

-- | What follows resolves in the area holding the player, while the players are separated.
focusAreaOf :: ReverseQueue m => PlayerId -> m ()
focusAreaOf = push . FocusArea

-- | The players of the area in focus join another area at the beginning of the travel phase.
joinAnotherPlayer :: ReverseQueue m => m ()
joinAnotherPlayer = push JoinAtTravel

-- | The card leaves the player's deck for their discard pile, as "search your deck for a copy
-- and discard it" reads.
discardFromDeck :: ReverseQueue m => PlayerId -> PlayerCard -> m ()
discardFromDeck pid card = do
  push $ RemoveFromDeck pid card.id
  push $ PutInDiscard card

{- | The character defends the enemy's attack on the player, exhausted or not, declared
from the window after the enemy is declared as an attacker, as Anchor Watch reads; it
defends in place of the player's own declaration.
-}
declareAsDefender :: (ReverseQueue m, Targetable target) => PlayerId -> EnemyId -> target -> m ()
declareAsDefender pid eid = push . DeclareAsDefender pid eid . toTarget

-- | The player may discard any number of cards from his hand, which the source reads
-- back as 'EachDiscardAnyFromHand' with no player left to ask, as Galdor of the Havens.
mayDiscardFromHand :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> m ()
mayDiscardFromHand source pid = push $ EachDiscardAnyFromHand (toSource source) [pid] []

-- | The player keeps his setup hand without being asked to mulligan, from the window
-- after drawing it, as Galdor of the Havens' response is taken instead of a mulligan.
skipMulligan :: (MonadTrans t, HasQueue Message m) => PlayerId -> t m ()
skipMulligan pid = cancelMatching \case
  Ask pid' (QuestionLabel l _ _ _) -> pid == pid' && l == ikey "label.keepStartingHand"
  _ -> False
-- * Sailing (Dream-chaser rules)

-- | Shift the heading one setting off-course, as Rolling Seas' Travel cost reads.
shiftHeadingOffCourse :: (ReverseQueue m, Sourceable source) => source -> m ()
shiftHeadingOffCourse source = push $ ShiftHeading (toSource source) ShiftOffCourse

-- | Shift the heading one setting on-course, as a sailing success icon does.
shiftHeadingOnCourse :: (ReverseQueue m, Sourceable source) => source -> m ()
shiftHeadingOnCourse source = push $ ShiftHeading (toSource source) ShiftOnCourse

-- | "Set your heading to the worst setting": shifted off-course until it is there.
setHeadingToWorst :: (ReverseQueue m, Sourceable source) => source -> m ()
setHeadingToWorst source = do
  heading <- getHeading
  for_ heading \h -> replicateM_ (fromEnum (maxBound :: Heading) - fromEnum h) (shiftHeadingOffCourse source)

-- | "Prepare the Corsair deck": each non-Ship enemy leaves the encounter deck for it.
prepareCorsairDeck :: ReverseQueue m => m ()
prepareCorsairDeck = push PrepareCorsairDeck

-- | "Players prepare their fleet", the heading starting on-course.
prepareFleet :: ReverseQueue m => m ()
prepareFleet = push PrepareFleet

-- | The Ship objective comes into play under the player, as The Beacon gives a set-aside
-- Dol Amroth Warship.
putShipIntoPlayUnder :: ReverseQueue m => PlayerId -> EncounterCard -> m ()
putShipIntoPlayUnder pid = push . PutObjectiveAllyIntoPlayUnder pid

-- | The player reveals the top card of the Corsair deck and engages it, for the source.
revealFromCorsairDeck :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> m ()
revealFromCorsairDeck source pid = push $ RevealFromCorsairDeck (toSource source) pid

-- | The player moves 1 resource from one of his heroes' pools to the card, as Umbar Raider
-- takes it; nothing, with no resources to move.
takeResourceFromAHero :: (ReverseQueue m, Sourceable a, Targetable a) => a -> PlayerId -> m ()
takeResourceFromAHero a pid = do
  heroes <- select (HeroControlledBy (PlayerWithId pid) <> HeroWithResources)
  chooseOrRunOneM pid do
    questionLabeled "takeResourceFromAHero"
    causedBy a
    targets heroes \h -> do
      removeTokens a h Resource 1
      placeResources a a 1

-- | Deal the damage to the Aflame location in play with the lowest Aflame value, the first
-- player choosing among those tied (Raid on the Grey Havens).
dealDamageToLowestAflame :: (ReverseQueue m, Sourceable source, Targetable source) => source -> Int -> m ()
dealDamageToLowestAflame source n = when (n > 0) do
  burning <- getAflameLocations
  for_ (minimumMay $ map snd burning) \lowest -> do
    lead <- getLead
    chooseOrRunOneM lead do
      questionLabeledWith "dealDamageToAflameLocation" [intVar "n" n]
      causedBy source
      targets [lid | (lid, x) <- burning, x == lowest] \lid -> dealDamage source lid n

-- | The player deals the damage to an Aflame location in play of their choice.
dealDamageToAnAflameLocation :: (ReverseQueue m, Sourceable source, Targetable source) => source -> PlayerId -> Int -> m ()
dealDamageToAnAflameLocation source pid n = do
  burning <- map fst <$> getAflameLocations
  chooseOrRunOneM pid do
    questionLabeledWith "dealDamageToAflameLocation" [intVar "n" n]
    causedBy source
    targets burning \lid -> dealDamage source lid n

-- | The shadow card dealt to the enemy goes to the staging area, as "Shadow: Add Burning
-- Piers to the staging area" reads of a location.
addShadowCardToStaging :: ReverseQueue m => EnemyId -> EncounterCard -> m ()
addShadowCardToStaging eid card = do
  push $ RemoveShadowCard eid card.id
  addToStaging card

{- | Turn the card over to its other side: a location, as an Uncharted one is flipped, or a
card whose other side is another kind of card, as Captain Sahír turns from enemy to
objective-ally and back (under the player his objective-ally side names), all tokens
removed.
-}
flipOver :: (ReverseQueue m, Targetable target) => target -> m ()
flipOver target = unlessM (elem CannotBeFlipped <$> getModifiers (toTarget target)) $ push $ Flip (toTarget target)

-- | As 'flipOver' a location, its progress kept, as Shrine to Morgoth floods.
flipOverKeepingProgress :: ReverseQueue m => LocationId -> m ()
flipOverKeepingProgress lid = unlessM (elem CannotBeFlipped <$> getModifiers (LocationTarget lid)) $ push $ FlipKeepingProgress lid

-- | The top cards of the Grotto deck go to the staging area, Grotto side faceup.
addFromGrottoDeck :: ReverseQueue m => Int -> m ()
addFromGrottoDeck n = when (n > 0) $ push $ AddFromGrottoDeck n

-- | The top card of the Grotto deck goes to the staging area, flipped to its Underwater side.
addUnderwaterFromGrottoDeck :: ReverseQueue m => m ()
addUnderwaterFromGrottoDeck = push AddUnderwaterFromGrottoDeck

-- | The card's ability resolves as if triggered, as Powerful Undertow resolves a location's
-- Forced effect "as if it is the end of the quest phase".
resolveAbility :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> Int -> m ()
resolveAbility pid source n = push $ UseCardAbility pid (toSource source) n []

-- | The player looks at the facedown side of the location; only he is shown it.
lookAtFacedownSide :: ReverseQueue m => PlayerId -> LocationId -> m ()
lookAtFacedownSide pid = push . LookAtFacedownSide pid

-- | The player chooses an Uncharted location in the staging area and looks at its
-- facedown side, as Steep Plateau's Response reads.
lookAtAnUnchartedLocation :: (ReverseQueue m, Targetable source) => PlayerId -> source -> m ()
lookAtAnUnchartedLocation pid source = do
  locations <- select (LocationInStagingArea <> unchartedLocation)
  chooseOrRunOneM pid do
    questionLabeled "lookAtFacedownSide"
    causedBy source
    targets locations (lookAtFacedownSide pid)

-- | The top cards of the Uncharted deck go to the staging area, Lost Island side faceup.
addFromUnchartedDeck :: ReverseQueue m => Int -> m ()
addFromUnchartedDeck n = when (n > 0) $ push $ AddFromUnchartedDeck n

{- | Remove all progress from each Uncharted location in the staging area and shuffle them
so that the players do not know which one is which.
-}
shuffleUnchartedLocations :: ReverseQueue m => m ()
shuffleUnchartedLocations = push ShuffleUnchartedLocations

-- | The card in the player's hand goes to the bottom of his deck.
putFromHandOnBottomOfDeck :: ReverseQueue m => PlayerId -> Card -> m ()
putFromHandOnBottomOfDeck pid card = push $ PutFromHandOnBottomOfDeck pid card.id

{- | Discard the bottom card of the player's deck, as "discard the bottom card of your deck.
If that card's printed cost is 2 or less, ..." reads: the card discarded, if any, for the
effect to read.
-}
discardBottomOfDeck :: ReverseQueue m => PlayerId -> m (Maybe Card)
discardBottomOfDeck pid = do
  bottom <- lastMay . (.deck) <$> getPlayer pid
  for_ bottom \case
    PlayerCard pc -> discardFromDeck pid pc
    _ -> pure ()
  pure bottom

-- | A card in the player's deck goes to the bottom of it, as one looked at from the top.
moveToBottomOfDeck :: ReverseQueue m => PlayerId -> Card -> m ()
moveToBottomOfDeck pid card = push $ MoveToBottomOfDeck pid card.id

-- | A destroyed ally, from its owner's discard pile, goes to the bottom of his deck, as
-- "place that ally on the bottom of its owner's deck" reads; an encounter card does not.
allyToBottomOfDeck :: ReverseQueue m => Card -> m ()
allyToBottomOfDeck = \case
  PlayerCard pc | Just owner <- pc.owner -> putDiscardOnBottomOfDeck owner pc.id
  _ -> pure ()

{- | Cancel this much of the damage about to be dealt to the target, from the window before
it is dealt, as Captain Sahír's resources cancel it point for point: the rest is still
dealt. See 'cancelDamage'.
-}
cancelSomeDamage
  :: (MonadTrans t, HasQueue Message m, HasQueue Message (t m), Targetable target) => target -> Int -> t m ()
cancelSomeDamage target k = do
  canceled <- lift $ popMessagesMatching \case
    DoStep 1 (DealDamage _ t _) -> t == toTarget target
    _ -> False
  for_ canceled \case
    DoStep 1 (DealDamage source t n) | n > k -> push $ DoStep 1 (DealDamage source t (n - k))
    _ -> pure ()

-- | The player discards each event in his hand.
discardEventsFromHand :: ReverseQueue m => PlayerId -> m ()
discardEventsFromHand = push . DiscardEventsFromHand

-- | The player spends 1 resource, of any sphere, from a hero he controls, as "Spend 1
-- resource from a hero you control" reads.
spendResourceFromAHero :: (ReverseQueue m, Sourceable source, Targetable source) => PlayerId -> source -> m ()
spendResourceFromAHero pid source = do
  heroes <- filterM (heroCanSpend <=< getHero) =<< select (HeroControlledBy (PlayerWithId pid) <> HeroWithResources)
  chooseOrRunOneM pid do
    questionLabeled "spendResourceFromAHero"
    causedBy source
    targets heroes \h -> removeTokens source h Resource 1
{- | The players as a group may spend resources of the sphere for what follows, the player
asked; without enough among them, they cannot.
-}
groupMaySpend
  :: (ReverseQueue m, Targetable source) => PlayerId -> source -> Sphere -> Int -> QueueT Message m () -> m ()
groupMaySpend pid source sphere n onSpend = groupMaySpendOr pid source sphere n onSpend (pure ())

-- | As 'groupMaySpend', with what follows if they do not, as "unless the players spend".
groupMaySpendOr
  :: (ReverseQueue m, Targetable source)
  => PlayerId -> source -> Sphere -> Int -> QueueT Message m () -> QueueT Message m () -> m ()
groupMaySpendOr pid source sphere n onSpend onDecline = do
  total <- sum <$> (traverse (`getResourcesOfSphere` sphere) =<< getRemainingPlayers)
  if total >= n
    then chooseOneM pid do
      questionLabeledWith "groupMaySpend" [intVar "count" n, textVar "sphere" (toLower $ tshow sphere)]
      causedBy source
      labeled "groupSpend" do
        spendGroupResources pid sphere n
        onSpend
      labeled "groupDoNotSpend" onDecline
    else pushAll =<< capture onDecline

-- | The damage about to be dealt to the target is lowered by so much, none left dealing none.
reduceDamage :: (MonadTrans t, HasQueue Message m, Targetable target) => target -> Int -> t m ()
reduceDamage (toTarget -> target) k = lift $ withQueue_ $ mapMaybe \case
  DoStep 1 (DealDamage s t n) | t == target -> guard (n > k) $> DoStep 1 (DealDamage s t (n - k))
  m -> Just m

{- | Cancel the enemy's attack: what is left of it does not resolve, though its end still
comes, for what lasts until then.
-}
cancelAttack :: (MonadTrans t, HasQueue Message m) => EnemyId -> t m ()
cancelAttack eid = lift $ withQueue_ $ mapMaybe \case
  ResolveShadow e _ _ | e == eid -> Nothing
  AttackDamage e _ | e == eid -> Nothing
  DefendWith _ e _ | e == eid -> Nothing
  DeclareDefenders _ e _ | e == eid -> Nothing
  DeclareSentinels _ e _ _ | e == eid -> Nothing
  DefendedByMany _ e _ | e == eid -> Nothing
  UndefendedAttack _ e | e == eid -> Nothing
  UndefendedDamage _ e | e == eid -> Nothing
  AttackDefended _ e _ | e == eid -> Nothing
  CheckWindows ws | any ending ws -> Just $ CheckWindows (filter (not . defended) ws)
  m -> Just m
 where
  ending = \case
    Window After (EnemyAttacking e _) -> e == eid
    _ -> False
  defended = \case
    Window _ (DefendedAttack _ e) -> e == eid
    Window _ (DeclaredAsDefender _ e) -> e == eid
    _ -> False

-- | Resolve the enemy's Grapple keyword, as Grasping Arm's shadow reads (The Thing in the
-- Depths).
resolveGrapple :: ReverseQueue m => EnemyId -> m ()
resolveGrapple = push . ResolveGrapple

-- | The enemy card, taken from where it was, resolves its Grapple keyword, as From the
-- Depths reads for the enemies it discards.
grappleCard :: ReverseQueue m => EncounterCard -> m ()
grappleCard = push . GrappleCard

-- | The shadow card dealt to the enemy leaves it to resolve its own Grapple keyword, as
-- Grasping Arm's shadow reads.
grappleShadowCard :: ReverseQueue m => EnemyId -> EncounterCard -> m ()
grappleShadowCard eid card = do
  removeShadowCard eid card.id
  grappleCard card

-- | The shadow card leaves the enemy it was dealt to, out of play, for the card taking it
-- to put somewhere, as Giant Arm's shadow takes itself.
removeShadowCard :: ReverseQueue m => EnemyId -> CardId -> m ()
removeShadowCard eid = push . RemoveShadowCard eid

-- | An enemy card, taken from where it was, comes into play engaged with the player, as
-- the enemy it returns, for the card putting it there to act on it after.
createEnemyEngaged :: ReverseQueue m => PlayerId -> EncounterCard -> m EnemyId
createEnemyEngaged pid card = do
  eid <- getRandom
  push $ CreateEnemyEngaged pid eid card
  pure eid

-- | The attachment card, from out of play, goes onto the host under the player without
-- being played, as a campaign card attaches a boon.
attachOutOfPlay :: (ReverseQueue m, Targetable host) => PlayerId -> PlayerCard -> host -> m ()
attachOutOfPlay pid card = push . PlayAttachmentOn pid card . toTarget

-- | The objective-ally card, from out of play, enters play under the player, as Mendor
-- under the first player.
putObjectiveAllyIntoPlayUnder :: ReverseQueue m => PlayerId -> EncounterCard -> m ()
putObjectiveAllyIntoPlayUnder pid = push . PutObjectiveAllyIntoPlayUnder pid

-- | In campaign mode, write to the campaign log the scenario holds, as Mysterious Omens'
-- boons join the Campaign Pool: kept if the scenario is won.
updateCampaignLog :: ReverseQueue m => CampaignLogUpdate -> m ()
updateCampaignLog = push . UpdateCampaignLog

-- | The player card, from out of play, is shuffled into its owner's deck, as Iârion's
-- Pendant once the players choose it.
shuffleIntoDeck :: ReverseQueue m => PlayerId -> PlayerCard -> m ()
shuffleIntoDeck pid card = do
  putInHand card
  push $ ShuffleFromHandIntoOwnersDeck pid card.id
-- | The shadow card dealt to the enemy is shuffled into the encounter deck, as "Shadow:
-- Shuffle Shadow Dweller back into the encounter deck" reads.
shuffleShadowCardIntoEncounterDeck :: ReverseQueue m => EnemyId -> EncounterCard -> m ()
shuffleShadowCardIntoEncounterDeck eid card = do
  push $ RemoveShadowCard eid card.id
  setAside card
  shuffleSetAsideIntoEncounterDeck [card]
-- | The card leaves the encounter deck and is revealed, as a search that reveals what it
-- finds reads (Save Us!).
revealFromEncounterDeck :: ReverseQueue m => EncounterCard -> m ()
revealFromEncounterDeck card = do
  removeFromEncounterDeck card
  push $ RevealCard card
