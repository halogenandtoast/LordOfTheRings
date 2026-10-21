-- | The game's verbs, for cards to push in the order they read. Each needs a
-- 'ReverseQueue', the queue a card runs its messages with (see
-- "LordOfTheRings.Classes.RunMessage"); a card can still 'push' a message no helper
-- covers, in the same order.
module LordOfTheRings.Message.Lifted (
  module LordOfTheRings.Message.Lifted,
  module X,
) where

import LordOfTheRings.Card (Card (PlayerCard), EncounterCard, PlayerCard)
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Effect
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Hero.Types ()
import LordOfTheRings.I18n
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted.Choose as X
import LordOfTheRings.Message.Lifted.Queue as X
import LordOfTheRings.Modifier
import LordOfTheRings.Outcome
import LordOfTheRings.Placement (Placement (ActiveLocation, AttachedTo, StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Queue
import LordOfTheRings.Source
import LordOfTheRings.Sphere (Sphere)
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (EnemyAttacking, ShadowEffectTriggered, Traveled))

dealDamage :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
dealDamage source target n = push $ DealDamage (toSource source) (toTarget target) n

healDamage :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
healDamage source target n = push $ HealDamage (toSource source) (toTarget target) n

placeTokens :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Token -> Int -> m ()
placeTokens source target token n = push $ PlaceTokens (toSource source) (toTarget target) token n

placeProgress :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
placeProgress source target = placeTokens source target Progress

placeResources :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m ()
placeResources source target = placeTokens source target Resource

removeTokens :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Token -> Int -> m ()
removeTokens source target token n = push $ RemoveTokens (toSource source) (toTarget target) token n

-- | Progress straight onto the current quest stage, past any active location.
placeProgressOnCurrentQuest :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeProgressOnCurrentQuest source n = traverse_ (\q -> placeProgress source q n) =<< getCurrentQuest

removeProgressFromQuest :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
removeProgressFromQuest source n = push $ RemoveProgressFromQuest (toSource source) n

raiseThreat :: (ReverseQueue m) => PlayerId -> Int -> m ()
raiseThreat pid n = push $ RaiseThreat pid n

lowerThreat :: (ReverseQueue m) => PlayerId -> Int -> m ()
lowerThreat pid n = push $ LowerThreat pid n

drawCards :: (ReverseQueue m) => PlayerId -> Int -> m ()
drawCards pid n = push $ DrawCards $ Drawing pid n

revealEncounterCard :: (ReverseQueue m) => m ()
revealEncounterCard = push RevealEncounterCard

revealEncounterCards :: (ReverseQueue m) => Int -> m ()
revealEncounterCards n = replicateM_ n revealEncounterCard

-- | Search the encounter deck for a matching card, add it to the staging area, and
-- shuffle the deck.
searchEncounterDeckToStaging :: (ReverseQueue m) => PlayerId -> CardMatcher -> m ()
searchEncounterDeckToStaging pid matcher = push $ Search pid EncounterDeck matcher ToStaging Shuffle

discard :: (ReverseQueue m, Targetable target) => target -> m ()
discard = push . Discard . toTarget

ready :: (ReverseQueue m, Targetable target) => target -> m ()
ready = push . Ready . toTarget

exhaust :: (ReverseQueue m, Targetable target) => target -> m ()
exhaust = push . Exhaust . toTarget

engageEnemy :: (ReverseQueue m) => PlayerId -> EnemyId -> m ()
engageEnemy pid eid = push $ EngageEnemy pid eid

gameOver :: (ReverseQueue m) => Outcome -> m ()
gameOver = push . GameOver

-- | The players win the game.
winGame :: (ReverseQueue m) => m ()
winGame = gameOver Won

createEffect :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> [ModifierType] -> EffectDuration -> m ()
createEffect source target modifiers duration = push $ CreateEffect $ Effect (toSource source) (toTarget target) modifiers duration

phaseModifiers :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> [ModifierType] -> m ()
phaseModifiers source target modifiers = createEffect source target modifiers UntilEndOfPhase

phaseModifier :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
phaseModifier source target = phaseModifiers source target . pure

roundModifiers :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> [ModifierType] -> m ()
roundModifiers source target modifiers = createEffect source target modifiers UntilEndOfRound

roundModifier :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
roundModifier source target = roundModifiers source target . pure

-- | For each player still in the game, in turn order from the first player.
eachPlayer :: (ReverseQueue m) => (PlayerId -> m ()) -> m ()
eachPlayer f = traverse_ f =<< getRemainingPlayers

-- | The player chooses a player still in the game, the card causing it marked; with one
-- player left there is no choice.
choosePlayerM :: (ReverseQueue m, Targetable source) => PlayerId -> source -> (PlayerId -> QueueT Message m ()) -> m ()
choosePlayerM pid source f = do
  players <- getRemainingPlayers
  chooseOrRunOneM pid do
    questionLabeled "choosePlayer"
    causedBy source
    targets players f

-- | Take matching messages off the game's queue, as cancelling an effect does. A card's
-- own queue holds only what it has pushed, so this reaches past it to the game's.
cancelMatching :: (MonadTrans t, HasQueue Message m) => (Message -> Bool) -> t m ()
cancelMatching = lift . void . popMessagesMatching

-- | Cancel the When Revealed effects of a card just revealed, and with them the lead's
-- click to resolve them.
cancelWhenRevealed :: (MonadTrans t, HasQueue Message m) => Target -> t m ()
cancelWhenRevealed t = cancelMatching \case
  WhenRevealed t' -> t == t'
  AskToResolveWhenRevealed t' _ -> t == t'
  _ -> False

-- | Heal all the damage on a character.
healAllDamage :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> m ()
healAllDamage source target = healDamage source target =<< getDamage (toTarget target)

-- | The player spends resources of a sphere from his heroes' pools.
spendResources :: (ReverseQueue m) => PlayerId -> Sphere -> Int -> m ()
spendResources pid sphere n = push $ SpendResources pid sphere n

removeFromDiscard :: (ReverseQueue m) => PlayerId -> CardId -> m ()
removeFromDiscard pid cid = push $ RemoveFromDiscard pid cid

placeEnemy :: (ReverseQueue m) => EnemyId -> Placement -> m ()
placeEnemy eid placement = push $ PlaceEnemy eid placement


-- | The player looks at the top card of the encounter deck; with @mayBottom@ they may
-- then move it to the bottom.
lookAtTopOfEncounterDeck :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> Bool -> m ()
lookAtTopOfEncounterDeck pid source mayBottom = push $ LookAtTopOfEncounterDeck pid (toSource source) mayBottom

-- | Return a card from the player's discard pile to their hand.
returnFromDiscardToHand :: (ReverseQueue m) => PlayerId -> CardId -> m ()
returnFromDiscardToHand pid cid = push $ ReturnFromDiscardToHand pid cid


-- | A shadow effect's two values: the first when the attack is undefended (no defender),
-- the second otherwise.
ifUndefended :: Maybe Target -> a -> a -> a
ifUndefended defender whenUndefended otherwise' = maybe whenUndefended (const otherwise') defender

-- | The enemy gets +N attack until its attack resolves, as "attacking enemy gets +1
-- attack" reads.
enemyGetsAttack :: (ReverseQueue m, Sourceable source) => source -> EnemyId -> Int -> m ()
enemyGetsAttack source eid n = attackModifier source eid (AttackModifier n)

-- | A modifier that lasts until the attack being made resolves, as a shadow effect's
-- "for this attack" does.
attackModifier :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
attackModifier source target m = createEffect source target [m] UntilEndOfAttack

-- | The player chooses and discards 1 of the cards.
chooseAndDiscard :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> [Target] -> m ()
chooseAndDiscard source pid cards = push $ ChooseAndDiscard (toSource source) pid cards

-- | Deal the enemy 1 more shadow card.
dealShadowCard :: (ReverseQueue m) => EnemyId -> m ()
dealShadowCard = push . DealShadowCard


-- | The player moves 1 resource from the pool of a hero they control to another hero's.
moveResourceFromHero :: (ReverseQueue m, Sourceable source, Targetable source) => PlayerId -> source -> m ()
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

addToStaging :: (ReverseQueue m) => EncounterCard -> m ()
addToStaging = push . AddToStaging

putQuestIntoPlay :: (ReverseQueue m) => EncounterCard -> m ()
putQuestIntoPlay = push . PutQuestIntoPlay

placeOrcGuard :: (ReverseQueue m) => PlayerId -> m ()
placeOrcGuard = push . PlaceOrcGuard

attachTreachery :: (ReverseQueue m, Targetable target) => TreacheryId -> target -> m ()
attachTreachery tid = push . AttachTreachery tid . toTarget

-- | The player chooses and exhausts N of their characters.
chooseAndExhaust :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> Int -> m ()
chooseAndExhaust source pid n = push $ ChooseAndExhaust (toSource source) pid n

-- | The player searches the top N cards of their deck for matching cards to take into
-- hand, any number of them or only 1, then shuffles their deck.
searchDeck :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> Int -> CardMatcher -> Bool -> m ()
searchDeck pid source n matcher anyNumber = do
  deck <- (.deck) <$> getPlayer pid
  push $ ChooseFromSearch pid (toSource source) (take n deck) matcher anyNumber

-- | The player searches the top N cards of their deck for up to the given number of
-- matching allies to put into play, each with the modifiers until the end of the round,
-- then shuffles their deck.
searchDeckIntoPlay :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> Int -> CardMatcher -> Int -> [ModifierType] -> m ()
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

discardTopOfDeck :: (ReverseQueue m) => PlayerId -> Int -> m ()
discardTopOfDeck pid n = push $ DiscardTopOfDeck pid n

shufflePlayerDeck :: (ReverseQueue m) => PlayerId -> m ()
shufflePlayerDeck = push . ShufflePlayerDeck

removeFromDeck :: (ReverseQueue m) => PlayerId -> CardId -> m ()
removeFromDeck pid cid = push $ RemoveFromDeck pid cid

-- | A card in the player's discard pile goes to the bottom of their deck.
putDiscardOnBottomOfDeck :: (ReverseQueue m) => PlayerId -> CardId -> m ()
putDiscardOnBottomOfDeck pid cid = push $ PutDiscardOnBottomOfDeck pid cid

-- | A card in the player's discard pile is shuffled into their deck.
shuffleFromDiscardIntoDeck :: (ReverseQueue m) => PlayerId -> CardId -> m ()
shuffleFromDiscardIntoDeck pid cid = push $ ShuffleFromDiscardIntoDeck pid cid

-- | An ally card, already taken from where it was, goes into play under the player's
-- control.
putAllyIntoPlayUnder :: (ReverseQueue m) => PlayerId -> PlayerCard -> m ()
putAllyIntoPlayUnder pid card = push $ PutAllyIntoPlayUnder pid card

-- | The player looks at the top N cards of the encounter deck, reveals 1 and discards
-- the rest.
revealOneOfTopEncounterCards :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> Int -> m ()
revealOneOfTopEncounterCards pid source n = push $ RevealOneOfTopEncounterCards pid (toSource source) n

-- | The player pays 1 resource of the sphere or discards the card; with nothing to pay,
-- the card is discarded.
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

-- | Messages to run once the enemy's attack has resolved, as "after this attack" reads.
afterAttack :: (MonadTrans t, HasQueue Message m) => EnemyId -> [Message] -> t m ()
afterAttack eid msgs = lift $ insertAfterMatching msgs \case
  CheckWindows ws -> any (\case Window After (EnemyAttacking eid' _) -> eid == eid'; _ -> False) ws
  _ -> False

-- | Cancel the shadow effects of the shadow cards whose effects are about to resolve in the
-- windows, as Hasty Stroke does.
cancelShadowEffects :: (MonadTrans t, HasQueue Message m) => [Window] -> t m ()
cancelShadowEffects windows =
  for_ [cid | Window _ (ShadowEffectTriggered cid _) <- windows] \cid ->
    cancelMatching \case
      ResolveShadowCard cid' _ _ _ -> cid == cid'
      _ -> False

returnToHand :: (ReverseQueue m, Targetable target) => target -> m ()
returnToHand = push . ReturnToHand . toTarget

destroy :: (ReverseQueue m, Targetable target) => target -> m ()
destroy = push . Destroy . toTarget

checkDefeated :: (ReverseQueue m, Targetable target) => target -> m ()
checkDefeated = push . CheckDefeated . toTarget

shuffleIntoEncounterDeck :: (ReverseQueue m, Targetable target) => target -> m ()
shuffleIntoEncounterDeck = push . ShuffleIntoEncounterDeck . toTarget

returnToTopOfEncounterDeck :: (ReverseQueue m, Targetable target) => target -> m ()
returnToTopOfEncounterDeck = push . ReturnToTopOfEncounterDeck . toTarget

shuffleEncounterDeck :: (ReverseQueue m) => m ()
shuffleEncounterDeck = push $ ShuffleDeck EncounterDeck

-- | Set an encounter card aside, out of play.
setAside :: (ReverseQueue m) => EncounterCard -> m ()
setAside = push . SetAside

addSetAsideToStaging :: (ReverseQueue m) => CardMatcher -> m ()
addSetAsideToStaging = push . AddSetAsideToStaging

shuffleEncounterDiscardIntoDeck :: (ReverseQueue m) => CardMatcher -> m ()
shuffleEncounterDiscardIntoDeck = push . ShuffleEncounterDiscardIntoDeck

-- | The player searches the encounter deck and discard pile for a matching card to add to
-- the staging area, then shuffles the encounter deck.
searchEncounterDeckAndDiscard :: (ReverseQueue m, Targetable source) => PlayerId -> source -> CardMatcher -> m ()
searchEncounterDeckAndDiscard pid source = push . SearchEncounterDeckAndDiscard pid (toTarget source)

-- | Claim the objective onto the host, attaching it there.
attachObjective :: (ReverseQueue m, Targetable host) => ObjectiveId -> host -> m ()
attachObjective oid = push . ClaimObjective oid . toTarget

-- | Move an attachment to another host.
moveAttachment :: (ReverseQueue m, Targetable host) => AttachmentId -> host -> m ()
moveAttachment aid = push . AttachCard aid . toTarget

putObjectiveAllyIntoPlay :: (ReverseQueue m) => ObjectiveId -> PlayerId -> m ()
putObjectiveAllyIntoPlay oid = push . PutObjectiveAllyIntoPlay oid

putHeroIntoPlay :: (ReverseQueue m) => PlayerId -> PlayerCard -> m ()
putHeroIntoPlay pid = push . PutHeroIntoPlay pid

-- | A treachery dealt as a shadow card comes into play attached to the host.
attachShadowTreachery :: (ReverseQueue m, Targetable host) => EnemyId -> CardId -> host -> m ()
attachShadowTreachery eid cid = push . PutShadowTreacheryIntoPlay eid cid . AttachedTo . toTarget

placeLocation :: (ReverseQueue m) => LocationId -> Placement -> m ()
placeLocation lid = push . PlaceLocation lid

-- | Make the location the active location, without traveling there.
makeActive :: (ReverseQueue m) => LocationId -> m ()
makeActive lid = placeLocation lid ActiveLocation

-- | Travel to the location without resolving its Travel effect: it becomes active, and
-- the players have traveled there.
travelWithoutCost :: (ReverseQueue m) => LocationId -> m ()
travelWithoutCost lid = do
  makeActive lid
  push $ CheckWindows [Window After (Traveled lid)]

-- | The quest deck goes back to the stage, followed by the stages given.
resetToStage :: (ReverseQueue m) => EncounterCard -> [EncounterCard] -> m ()
resetToStage card = push . ResetToStage card

checkStage :: (ReverseQueue m) => QuestId -> m ()
checkStage = push . CheckStage

-- | The player switches a card in hand with one in their deck.
swapHandWithDeck :: (ReverseQueue m) => PlayerId -> Card -> Card -> m ()
swapHandWithDeck pid fromHand fromDeck = push $ SwapHandWithDeck pid fromHand.id fromDeck.id

-- | Run the message again, as a later step of itself.
doStep :: (ReverseQueue m) => Int -> Message -> m ()
doStep n = push . DoStep n

-- | Until the end of the phase, damage from the enemy's undefended attacks goes to the
-- character.
undefendedDamageTo :: (ReverseQueue m, Sourceable source, Targetable target) => source -> EnemyId -> target -> m ()
undefendedDamageTo source eid = phaseModifier source eid . UndefendedDamageTo . toTarget

-- | The card leaves the encounter discard pile and is revealed, its When Revealed
-- effects resolving.
revealFromEncounterDiscard :: (ReverseQueue m) => EncounterCard -> m ()
revealFromEncounterDiscard card = do
  push $ RemoveFromEncounterDiscard card
  push $ RevealCard card

-- | After its attack resolves, the enemy returns to the staging area.
returnToStagingAfterAttack :: (MonadTrans t, HasQueue Message m) => EnemyId -> t m ()
returnToStagingAfterAttack eid = afterAttack eid [PlaceEnemy eid StagingArea]

-- | The defending character is removed from the attack, which is now undefended.
makeUndefended :: (MonadTrans t, HasQueue Message m, ReverseQueue (t m)) => PlayerId -> EnemyId -> t m ()
makeUndefended pid eid = do
  cancelMatching \case
    AttackDamage eid' _ -> eid == eid'
    DefendedByMany _ eid' _ -> eid == eid'
    _ -> False
  push $ UndefendedDamage pid eid

discardFromHand :: (ReverseQueue m) => PlayerId -> Card -> m ()
discardFromHand pid card = push $ DiscardFromHand pid card.id

-- | The player makes an escape test against the given number of encounter cards,
-- counting the stat; what follows a failure and a pass are actions of their own.
escapeTest
  :: (ReverseQueue m, Sourceable source)
  => PlayerId -> source -> Int -> EscapeStat -> QueueT Message m () -> QueueT Message m () -> m ()
escapeTest pid source n stat onFail onPass = do
  failed <- capture onFail
  passed <- capture onPass
  push $ MakeEscapeTest $ EscapeTest pid (toSource source) n stat [] failed passed

placeObjective :: (ReverseQueue m) => ObjectiveId -> Placement -> m ()
placeObjective oid = push . PlaceObjective oid

-- | The card goes facedown under the ally.
attachFacedown :: (ReverseQueue m) => AllyId -> PlayerCard -> m ()
attachFacedown aid = push . AttachFacedown aid

-- | Discard a shadow card dealt to the enemy, unresolved.
discardShadowCard :: (ReverseQueue m) => EnemyId -> EncounterCard -> m ()
discardShadowCard eid card = do
  push $ RemoveShadowCard eid card.id
  push $ AddToEncounterDiscard [card]

-- | The encounter card leaves the discard pile for the top of the encounter deck.
moveToTopOfEncounterDeck :: (ReverseQueue m) => EncounterCard -> m ()
moveToTopOfEncounterDeck card = do
  push $ RemoveFromEncounterDiscard card
  push $ PutOnTopOfEncounterDeck card

-- | The shadow card, a location, becomes the active location.
putShadowLocationIntoPlay :: (ReverseQueue m) => EnemyId -> CardId -> m ()
putShadowLocationIntoPlay eid = push . PutShadowLocationIntoPlay eid

-- | The player returns a matching ally they control to their hand, as "return a Silvan
-- ally you control to your hand" pays for a card; see 'controlsAlly'.
returnAllyToHand :: (ReverseQueue m, Targetable source) => PlayerId -> source -> AllyMatcher -> m ()
returnAllyToHand pid source matcher = do
  allies <- select (AllyControlledBy (PlayerWithId pid) <> matcher)
  chooseOrRunOneM pid do
    questionLabeled "returnAllyToHand"
    causedBy source
    targets allies returnToHand

-- | Play a card from the player's hand without paying its cost: it still counts as played
-- from hand.
playFromHandAtNoCost :: (ReverseQueue m) => PlayerId -> CardId -> m ()
playFromHandAtNoCost pid cid = push $ PlayFromHandAtNoCost pid cid

-- | The character attacks the enemy, wherever it is, as an effect declares it an
-- attacker; its attack raised by the bonus for that attack alone.
attackEnemyWith :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> Target -> EnemyId -> Int -> m ()
attackEnemyWith pid source attacker eid bonus = do
  push $ AttackWith attacker
  when (bonus /= 0) $ createEffect source attacker [AttackModifier bonus] UntilEndOfAttack
  push $ ResolveAttack pid eid

-- | The player chooses and discards N cards from their hand.
chooseAndDiscardFromHand :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> Int -> m ()
chooseAndDiscardFromHand pid source n = push $ ChooseAndDiscardFromHand pid (toSource source) n
