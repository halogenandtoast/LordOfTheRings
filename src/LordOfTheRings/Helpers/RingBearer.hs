{- | The Lord of the Rings saga's One Ring and its Ring-bearer: the objective the scenario
attaches to its Fellowship hero (see 'LordOfTheRings.Saga.fellowshipSetup'), which Frodo
Baggins and some encounter cards exhaust as a cost, and the cards whose leaving play loses
the game.
-}
module LordOfTheRings.Helpers.RingBearer (
  theOneRingCodes,
  heroWithReadyTheOneRing,
  sagaOneRingReady,
  exhaustTheOneRing,
  leavesPlay,
) where

import LordOfTheRings.Card (CardCode, toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Criteria (Criterion (HeroExists))
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted (exhaust)
import LordOfTheRings.Message.Lifted.Queue (ReverseQueue)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Target

-- | The saga's One Ring objectives: The Black Riders', The Road Darkens', The Land of
-- Shadow's and The Mountain of Fire's.
theOneRingCodes :: [CardCode]
theOneRingCodes = ["141085", "142091", "144063", "146065"]

-- | The hero The One Ring is attached to, the ring ready.
heroWithReadyTheOneRing :: HeroMatcher
heroWithReadyTheOneRing = HeroMatchAny [HeroWithReadyAttachment code | code <- theOneRingCodes]

-- | "Exhaust The One Ring" can be paid.
sagaOneRingReady :: Criterion
sagaOneRingReady = HeroExists heroWithReadyTheOneRing

-- | Exhaust The One Ring, as a cost paid as the effect resolves, its criteria
-- ('sagaOneRingReady') having checked it is ready.
exhaustTheOneRing :: ReverseQueue m => m ()
exhaustTheOneRing = do
  rings <- getsGame (toList . entitiesObjectives . gameEntities)
  for_ (take 1 [r | r <- rings, toCardCode r `elem` theOneRingCodes, not r.exhausted, isAttached r.placement]) \r ->
    exhaust (ObjectiveTarget r.id)
 where
  isAttached = \case
    AttachedTo _ -> True
    _ -> False

{- | The message takes the card out of play, as "If X leaves play, the players lose the
game" reads: discarded or destroyed, removed from the game, returned to hand, shuffled or
put into a deck, set aside.
-}
leavesPlay :: Target -> Message -> Bool
leavesPlay t = \case
  Discard t' -> t == t'
  RemoveFromGame t' -> t == t'
  ReturnToHand t' -> t == t'
  ShuffleIntoOwnersDeck t' -> t == t'
  PutOnBottomOfOwnersDeck t' -> t == t'
  ShuffleIntoEncounterDeck t' -> t == t'
  ReturnToTopOfEncounterDeck t' -> t == t'
  ReturnToBottomOfEncounterDeck t' -> t == t'
  SetAsideHero hid -> t == HeroTarget hid
  _ -> False
