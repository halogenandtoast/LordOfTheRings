{- | What The Lord of the Rings saga's Fellowship cards share: The One Ring objective of
each box ('LordOfTheRings.Saga.oneRings'), the Ring-bearer, and the cards whose leaving
play loses the game.
-}
module LordOfTheRings.Helpers.Fellowship (module LordOfTheRings.Helpers.Fellowship) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Criteria (Criterion (HeroExists))
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message (Message (..))
import LordOfTheRings.Message.Lifted (exhaust)
import LordOfTheRings.Message.Lifted.Queue (ReverseQueue)
import LordOfTheRings.Objective.Types (Objective)
import LordOfTheRings.Placement (Placement (AttachedTo, Unplaced))
import LordOfTheRings.Prelude
import LordOfTheRings.Saga (oneRings)
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (RingBearer))

-- | The hero with the Ring-bearer trait, printed or gained, as "Attach to the
-- Ring-bearer" reads.
ringBearer :: HeroMatcher
ringBearer = HeroWithTrait RingBearer

-- | The hero The One Ring is attached to, the ring ready.
heroWithReadyTheOneRing :: HeroMatcher
heroWithReadyTheOneRing = HeroMatchAny [HeroWithReadyAttachment code | code <- oneRings]

-- | "Exhaust The One Ring" can be paid.
sagaOneRingReady :: Criterion
sagaOneRingReady = HeroExists heroWithReadyTheOneRing

-- | Exhaust The One Ring, as a cost paid as the effect resolves, its criteria
-- ('sagaOneRingReady') having checked it is ready.
exhaustTheOneRing :: ReverseQueue m => m ()
exhaustTheOneRing = do
  rings <- getsGame (filter isReadyRing . toList . entitiesObjectives . gameEntities)
  for_ (take 1 [r | r <- rings, isAttached r.placement]) \r -> exhaust (ObjectiveTarget r.id)
 where
  isAttached = \case
    AttachedTo _ -> True
    _ -> False

-- | The saga One Ring the player controls, ready to exhaust: it is controlled by whoever
-- controls the hero it is attached to (FAQ 1.38).
readySagaOneRing :: HasGame m => PlayerId -> m (Maybe ObjectiveId)
readySagaOneRing pid = do
  rings <- getsGame (filter isReadyRing . toList . entitiesObjectives . gameEntities)
  fmap (fmap toId . headMay) $ flip filterM rings \o -> case o.placement of
    AttachedTo (HeroTarget hid) -> selectAny (HeroWithId hid <> HeroControlledBy (PlayerWithId pid))
    _ -> pure False

-- | The saga's One Ring in play, whichever box it is from, as "exhaust The One Ring" reads.
getSagaOneRing :: HasGame m => m (Maybe Objective)
getSagaOneRing = getsGame (find isInPlayRing . toList . entitiesObjectives . gameEntities)
 where
  isInPlayRing o = toCardCode o `elem` oneRings && o.placement /= Unplaced

-- | Whether The One Ring is in play and exhausted, as Morgul Wraith reads.
theOneRingExhausted :: HasGame m => m Bool
theOneRingExhausted = maybe False (.exhausted) <$> getSagaOneRing

isReadyRing :: Objective -> Bool
isReadyRing o = toCardCode o `elem` oneRings && not o.exhausted

{- | Whether the message takes the card out of play, however it goes: discarded, as a hero
destroyed is, removed from the game, returned to a hand, shuffled or put into a deck, or set
aside. "If X leaves play, the players lose the game" reads each of these.
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
