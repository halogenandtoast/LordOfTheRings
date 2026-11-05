-- | What A Shadow of the Past's cards share: Hide tests failed this phase or round, the
-- Nazgûl each stage has engage a player who fails one, and the Black Rider stages 2B and
-- 3B reveal.
module LordOfTheRings.Scenario.Scenarios.AShadowOfThePast.Helpers (
  failedHideTestThisPhase,
  failedHideTestThisRound,
  nazgulEngage,
  revealBlackRider,
) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier (ModifierType (FailedHideTestThisPhase, FailedHideTestThisRound))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Types (QuestAttrs)
import LordOfTheRings.Trait (Trait (Nazgul))

-- | "If you have failed a Hide test this phase", as Crawling Towards Him reads.
failedHideTestThisPhase :: HasGame m => PlayerId -> m Bool
failedHideTestThisPhase pid = elem FailedHideTestThisPhase <$> getModifiers pid

-- | "If you have failed a Hide test this round", as Evil Crow's shadow reads.
failedHideTestThisRound :: HasGame m => PlayerId -> m Bool
failedHideTestThisRound pid = elem FailedHideTestThisRound <$> getModifiers pid

-- | "When a player fails a Hide test, each Nazgûl enemy in the staging area engages that
-- player. (Trigger Forced effects after resolving this effect.)": as the failure resolves,
-- before the window after it.
nazgulEngage :: ReverseQueue m => PlayerId -> m ()
nazgulEngage pid = traverse_ (engageEnemy pid) =<< select (EnemyWithTrait Nazgul <> EnemyInStagingArea)

-- | When Revealed: The first player searches the encounter deck and discard pile for a
-- Black Rider, reveals it (cannot be canceled), and adds it to the staging area. Shuffle the
-- encounter deck.
revealBlackRider :: ReverseQueue m => QuestAttrs -> m ()
revealBlackRider attrs = do
  lead <- getLead
  cannotBeCanceled attrs $ searchEncounterDeckAndDiscardToReveal lead attrs (cardIs Enemies.blackRider)
