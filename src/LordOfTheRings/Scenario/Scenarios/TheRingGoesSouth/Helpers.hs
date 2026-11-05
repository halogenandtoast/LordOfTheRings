{- | What The Ring Goes South's cards share: damage on the active locations, which each
location's Forced effect counts as it is explored, and what the players do as a group.

With two active locations (stage 4B), an effect on "the active location" reaches one of
them, the first player choosing (FAQ 1.34).
-}
module LordOfTheRings.Scenario.Scenarios.TheRingGoesSouth.Helpers (
  damageAnActiveLocation,
  damageEachActiveLocation,
  exhaustCharactersAsGroup,
  discardAlliesAsGroup,
  firstPlayerHasRingBearer,
) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (RingBearer))

-- | Place damage on the active location, the first player choosing one of two.
damageAnActiveLocation :: (ReverseQueue m, Sourceable source, Targetable source) => source -> Int -> m ()
damageAnActiveLocation source n = do
  lead <- getLead
  actives <- select IsActiveLocation
  chooseOrRunOneM lead do
    questionLabeled "theRingGoesSouth.activeLocation"
    causedBy source
    targets actives \l -> placeDamage source l n

-- | Place damage on each active location.
damageEachActiveLocation :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
damageEachActiveLocation source n = select IsActiveLocation >>= traverse_ \l -> placeDamage source l n

-- | The players, as a group, exhaust this many ready characters in play, one at a time: the
-- first player, who has the final say, picks them.
exhaustCharactersAsGroup :: (ReverseQueue m, Targetable source) => source -> Int -> m ()
exhaustCharactersAsGroup source n = when (n > 0) do
  lead <- getLead
  able <- selectCharacters (ReadyHero <> HeroControlledBy AnyPlayer) (ReadyAlly <> AllyControlledBy AnyPlayer)
  chooseOrRunOneM lead do
    questionLabeled "theRingGoesSouth.exhaustCharacter"
    causedBy source
    targets able \c -> do
      exhaust c
      exhaustCharactersAsGroup source (n - 1)

-- | The players, as a group, discard this many allies from play, one at a time: the first
-- player, who has the final say, picks them.
discardAlliesAsGroup :: (ReverseQueue m, Targetable source, Sourceable source) => source -> Int -> m ()
discardAlliesAsGroup source n = when (n > 0) do
  lead <- getLead
  allies <- select (AllyControlledBy AnyPlayer)
  chooseOrRunOneM lead do
    questionLabeled "theRingGoesSouth.discardAlly"
    causedBy source
    targets allies \(a :: AllyId) -> do
      discard source a
      discardAlliesAsGroup source (n - 1)

-- | Whether the first player controls the Ring-bearer: not while he is facedown under
-- Watcher in the Water, as no one controls him then.
firstPlayerHasRingBearer :: HasGame m => m Bool
firstPlayerHasRingBearer = do
  lead <- getLead
  selectAny (HeroWithTrait RingBearer <> HeroControlledBy (PlayerWithId lead))
