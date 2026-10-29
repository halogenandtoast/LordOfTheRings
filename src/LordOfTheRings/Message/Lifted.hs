{- | The game's verbs, for cards to push in the order they read. Each needs a
'ReverseQueue', the queue a card runs its messages with (see
"LordOfTheRings.Classes.RunMessage"); a card can still 'push' a message no helper
covers, in the same order.
-}
module LordOfTheRings.Message.Lifted (
  module LordOfTheRings.Message.Lifted,
  module X,
) where

import LordOfTheRings.Card (Card (..), CardDef, EncounterCard, HasCardCode (..), IsCard (..), PlayerCard)
import LordOfTheRings.Classes.Entity (Entity (toAttrs))
import LordOfTheRings.Scenario.Types ()
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Effect
import LordOfTheRings.Entities (entitiesLocations, entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities, gamePhase)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Hero.Types ()
import LordOfTheRings.I18n (intVar)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted.Choose as X
import LordOfTheRings.Message.Lifted.Queue as X
import LordOfTheRings.Modifier
import LordOfTheRings.Outcome
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Placement (
  Placement (ActiveLocation, AttachedTo, StagingArea),
  placementController,
 )
import LordOfTheRings.Prelude hiding (count)
import LordOfTheRings.Queue
import LordOfTheRings.Source
import LordOfTheRings.Sphere (Sphere)
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window (
  Timing (After, When),
  Window (..),
  WindowType (EnemyAttacking, ShadowEffectTriggered, StagingStepEnds, ThreatRaisedByDoomed, ThreatRaisedByEncounter, Traveled),
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

raiseThreat :: ReverseQueue m => PlayerId -> Int -> m ()
raiseThreat pid n = push $ RaiseThreat pid n

{- | Raise the player's threat by an encounter or quest card's effect, opening the window
Elfhelm answers.
-}
encounterRaisesThreat :: ReverseQueue m => PlayerId -> Int -> m ()
encounterRaisesThreat pid n = when (n > 0) do
  push $ RaiseThreat pid n
  push $ CheckWindows [Window After $ ThreatRaisedByEncounter pid]

{- | Each player raises his threat by N from the Doomed keyword, opening his window for
it, as a player card with Doomed N played does; Close Call reads it with its X.
-}
raiseThreatFromDoomed :: ReverseQueue m => Int -> m ()
raiseThreatFromDoomed n = when (n > 0) $ eachPlayer \pid -> do
  raiseThreat pid n
  push $ CheckWindows [Window After $ ThreatRaisedByDoomed pid]

lowerThreat :: ReverseQueue m => PlayerId -> Int -> m ()
lowerThreat pid n = push $ LowerThreat pid n

drawCards :: ReverseQueue m => PlayerId -> Int -> m ()
drawCards pid n = push $ DrawCards $ Drawing pid n

revealEncounterCard :: ReverseQueue m => m ()
revealEncounterCard = push RevealEncounterCard

revealEncounterCards :: ReverseQueue m => Int -> m ()
revealEncounterCards n = replicateM_ n revealEncounterCard

{- | Search the encounter deck for a matching card, add it to the staging area, and
shuffle the deck.
-}
searchEncounterDeckToStaging :: ReverseQueue m => PlayerId -> CardMatcher -> m ()
searchEncounterDeckToStaging pid matcher = push $ Search pid EncounterDeck matcher ToStaging Shuffle

discard :: (ReverseQueue m, Targetable target) => target -> m ()
discard = push . Discard . toTarget

ready :: (ReverseQueue m, Targetable target) => target -> m ()
ready = push . Ready . toTarget

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
cancelWhenRevealed :: (MonadTrans t, HasQueue Message m) => Target -> t m ()
cancelWhenRevealed t = cancelMatching \case
  WhenRevealed t' -> t == t'
  AskToResolveWhenRevealed t' _ -> t == t'
  _ -> False

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

removeFromDiscard :: ReverseQueue m => PlayerId -> CardId -> m ()
removeFromDiscard pid cid = push $ RemoveFromDiscard pid cid

placeEnemy :: ReverseQueue m => EnemyId -> Placement -> m ()
placeEnemy eid placement = push $ PlaceEnemy eid placement

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

attachTreachery :: (ReverseQueue m, Targetable target) => TreacheryId -> target -> m ()
attachTreachery tid = push . AttachTreachery tid . toTarget

-- | The player chooses and exhausts N of their characters.
chooseAndExhaust :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> Int -> m ()
chooseAndExhaust source pid n = push $ ChooseAndExhaust (toSource source) pid n

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

removeFromDeck :: ReverseQueue m => PlayerId -> CardId -> m ()
removeFromDeck pid cid = push $ RemoveFromDeck pid cid

-- | A card in the player's discard pile goes to the bottom of their deck.
putDiscardOnBottomOfDeck :: ReverseQueue m => PlayerId -> CardId -> m ()
putDiscardOnBottomOfDeck pid cid = push $ PutDiscardOnBottomOfDeck pid cid

-- | A card in the player's discard pile is shuffled into their deck.
shuffleFromDiscardIntoDeck :: ReverseQueue m => PlayerId -> CardId -> m ()
shuffleFromDiscardIntoDeck pid cid = push $ ShuffleFromDiscardIntoDeck pid cid

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

addToVictoryDisplay :: (ReverseQueue m, Targetable target) => target -> m ()
addToVictoryDisplay = push . AddToVictoryDisplay . toTarget

destroy :: (ReverseQueue m, Targetable target) => target -> m ()
destroy = push . Destroy . toTarget

checkDefeated :: (ReverseQueue m, Targetable target) => target -> m ()
checkDefeated = push . CheckDefeated . toTarget

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

-- | The card leaves play for good: an objective or a location.
removeFromGame :: (ReverseQueue m, Targetable target) => target -> m ()
removeFromGame = push . RemoveFromGame . toTarget

-- | The character is shuffled into its owner's deck, as Lost and Alone does with a hero.
shuffleIntoOwnersDeck :: (ReverseQueue m, Targetable target) => target -> m ()
shuffleIntoOwnersDeck = push . ShuffleIntoOwnersDeck . toTarget

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

-- | The card goes facedown under the ally, or the Nameless enemy.
attachFacedown :: (ReverseQueue m, Targetable target) => target -> PlayerCard -> m ()
attachFacedown target = push . AttachFacedown (toTarget target)

-- | Discard a shadow card dealt to the enemy, unresolved.
discardShadowCard :: ReverseQueue m => EnemyId -> EncounterCard -> m ()
discardShadowCard eid card = do
  push $ RemoveShadowCard eid card.id
  push $ AddToEncounterDiscard [card]

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
  cancelMatching \case
    RevealShadowCard eid' _ -> eid' == eid
    ResolveShadowCard _ eid' _ _ -> eid' == eid
    CheckWindows (Window When (ShadowEffectTriggered _ eid') : _) -> eid' == eid
    _ -> False
  resolveShadowCards eid pid defenders

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
-- immune to card effects.
removeAllProgress :: (ReverseQueue m, Sourceable source) => source -> m ()
removeAllProgress source = do
  quests <- getsGame (toList . entitiesQuests . gameEntities)
  for_ quests \q -> when (q.progress > 0) $ removeTokens source q Progress q.progress
  locations <- getsGame (toList . entitiesLocations . gameEntities)
  for_ locations \l -> do
    immune <- elem ImmuneToCardEffects <$> getModifiers l
    when (l.progress > 0 && not immune) $ removeTokens source l Progress l.progress

-- | The player discards every card in their hand.
discardHand :: ReverseQueue m => PlayerId -> m ()
discardHand pid = traverse_ (discardFromHand pid) . (.hand) =<< getPlayer pid

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

-- | Place the ally elsewhere, as Captured! puts Faramir under itself.
placeAlly :: ReverseQueue m => AllyId -> Placement -> m ()
placeAlly aid = push . PlaceAlly aid

-- | The enemy makes an additional attack against the player once this attack is over, dealt
-- a shadow card for it, as Lord Alcaron and Heart of Fangorn's shadow read.
additionalAttack :: PlayerId -> EnemyId -> QueueT Message GameT ()
additionalAttack pid eid = do
  again <- capture $ immediateAttack pid eid
  afterAttack eid again
