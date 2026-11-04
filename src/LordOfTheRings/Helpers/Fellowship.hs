{- | What The Lord of the Rings saga's Fellowship cards share: The One Ring objective of
each box, the Ring-bearer, and the cards whose leaving play loses the game.
-}
module LordOfTheRings.Helpers.Fellowship (module LordOfTheRings.Helpers.Fellowship) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Classes.Entity (toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message (Message (..))
import LordOfTheRings.Objective.Types (Objective)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (RingBearer))

-- | The One Ring objectives of The Black Riders, The Road Darkens, The Land of Shadow and
-- The Mountain of Fire, attached to the Ring-bearer at setup.
sagaOneRings :: [CardCode]
sagaOneRings = ["141085", "142091", "144063", "146065"]

-- | The hero with the Ring-bearer trait, printed or gained, as "Attach to the
-- Ring-bearer" reads.
ringBearer :: HeroMatcher
ringBearer = HeroWithTrait RingBearer

-- | The saga One Ring the player controls, ready to exhaust: it is controlled by whoever
-- controls the hero it is attached to (FAQ 1.38).
readySagaOneRing :: HasGame m => PlayerId -> m (Maybe ObjectiveId)
readySagaOneRing pid = do
  rings <- getsGame (filter isReadyRing . toList . entitiesObjectives . gameEntities)
  fmap (fmap toId . headMay) $ flip filterM rings \o -> case o.placement of
    AttachedTo (HeroTarget hid) -> selectAny (HeroWithId hid <> HeroControlledBy (PlayerWithId pid))
    _ -> pure False
 where
  isReadyRing :: Objective -> Bool
  isReadyRing o = toCardCode o `elem` sagaOneRings && not o.exhausted

{- | Whether the message takes the card out of play, however it goes: discarded, as a hero
destroyed is, removed from the game, or returned to a hand or a deck. "If Aragorn leaves
play, the players lose the game" reads each of these.
-}
leavesPlay :: Target -> Message -> Bool
leavesPlay t = \case
  Discard t' -> t == t'
  RemoveFromGame t' -> t == t'
  ReturnToHand t' -> t == t'
  ShuffleIntoOwnersDeck t' -> t == t'
  PutOnBottomOfOwnersDeck t' -> t == t'
  ReturnToTopOfEncounterDeck t' -> t == t'
  ReturnToBottomOfEncounterDeck t' -> t == t'
  _ -> False
