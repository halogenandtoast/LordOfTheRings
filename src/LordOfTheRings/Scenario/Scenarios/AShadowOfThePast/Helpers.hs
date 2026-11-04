-- | What A Shadow of the Past's cards share: Hide tests failed this phase or round, the
-- Nazgûl each stage has engage a player who fails one, and the Black Rider stages 2B and
-- 3B reveal.
module LordOfTheRings.Scenario.Scenarios.AShadowOfThePast.Helpers (
  failedHideTestThisPhase,
  failedHideTestThisRound,
  nazgulEngage,
  revealBlackRider,
  winAShadowOfThePast,
) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogUpdate (AddToPool), PoolCard (..))
import LordOfTheRings.Card (CardDef, toCardCode)
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Helpers.Campaign (getCampaignLog)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Trait (Trait (RingBearer))
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

{- | After Bucklebury Ferry leaves play as an explored location, the players win the game. In
campaign mode the campaign card's resolution is chosen as they do, the game asking nothing
once it is over: "Choose Gandalf's Delay or The Ring Draws Them and add it to the Campaign
Pool. Then, choose Gildor Inglorion or Mr. Underhill and add it to the Campaign Pool. The
chosen cards have been earned by the players." Mr. Underhill, Permanent, is recorded on
the Ring-bearer (The Black Riders rules insert, "Permanent").
-}
winAShadowOfThePast :: ReverseQueue m => m ()
winAShadowOfThePast = do
  campaign <- getCampaignLog
  for_ campaign \_ -> do
    lead <- getLead
    ringBearer <- traverse getHero . headMay =<< select (HeroWithTrait RingBearer)
    chooseOneM lead do
      questionLabeled "aShadowOfThePast.chooseBurden"
      labeled "aShadowOfThePast.gandalfsDelay" $ earn Objectives.gandalfsDelay Nothing
      labeled "aShadowOfThePast.theRingDrawsThem" $ earn Treacheries.theRingDrawsThem Nothing
    chooseOneM lead do
      questionLabeled "aShadowOfThePast.chooseBoon"
      labeled "aShadowOfThePast.gildorInglorion" $ earn Objectives.gildorInglorionObjective Nothing
      labeled "aShadowOfThePast.mrUnderhill" $ earn Attachments.mrUnderhill (toCardCode <$> ringBearer)
  winGame
 where
  earn :: ReverseQueue n => CardDef -> Maybe CardCode -> n ()
  earn def hero = updateCampaignLog $ AddToPool $ PoolCard (toCardCode def) Nothing hero
