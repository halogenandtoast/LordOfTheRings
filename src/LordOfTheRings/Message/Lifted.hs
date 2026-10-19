-- | The game's verbs, for cards to push in the order they read. Each needs a
-- 'ReverseQueue', the queue a card runs its messages with (see
-- "LordOfTheRings.Classes.RunMessage"); a card can still 'push' a message no helper
-- covers, in the same order.
module LordOfTheRings.Message.Lifted (
  module LordOfTheRings.Message.Lifted,
  module X,
) where

import LordOfTheRings.Card (EncounterCard)
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Effect
import LordOfTheRings.I18n
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted.Choose as X
import LordOfTheRings.Message.Lifted.Queue as X
import LordOfTheRings.Modifier
import LordOfTheRings.Outcome
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Queue
import LordOfTheRings.Source
import LordOfTheRings.Sphere (Sphere)
import LordOfTheRings.Target
import LordOfTheRings.Token

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

-- | The enemy gets +N attack until the end of the phase, as "attacking enemy gets +1
-- attack" reads.
enemyGetsAttack :: (ReverseQueue m, Sourceable source) => source -> EnemyId -> Int -> m ()
enemyGetsAttack source eid n = phaseModifier source eid (AttackModifier n)

-- | The player chooses and discards 1 of the cards.
chooseAndDiscard :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> [Target] -> m ()
chooseAndDiscard source pid cards = push $ ChooseAndDiscard (toSource source) pid cards

-- | Deal the enemy 1 more shadow card.
dealShadowCard :: (ReverseQueue m) => EnemyId -> m ()
dealShadowCard = push . DealShadowCard


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
