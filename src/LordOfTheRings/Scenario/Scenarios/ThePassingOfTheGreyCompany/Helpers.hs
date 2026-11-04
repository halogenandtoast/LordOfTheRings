-- | What The Passing of the Grey Company's cards share: Overcome by Fear on the players'
-- threat dials, and the threat of 35 its cards read.
module LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (
  overcomeByFearOn,
  attachOvercomeByFearOr,
  attachOvercomeByFearOrDiscardAlly,
  hasThreatAtLeast35,
  theDeadAreFollowing,
  oathbreakerEnemy,
  mayRaiseThreatToSearchForAnduril,
) where

import LordOfTheRings.Card (CardType (EnemyType), EncounterCard, toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities (entitiesObjectives, entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (ObjectiveAttrs)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Message (Message)
import LordOfTheRings.Trait (Trait (Oathbreaker))
import LordOfTheRings.Source (Sourceable)
import LordOfTheRings.Target

-- | The copy of Overcome by Fear on the player's threat dial, if he has one (Limit 1 per
-- threat dial).
overcomeByFearOn :: HasGame m => PlayerId -> m [ObjectiveId]
overcomeByFearOn pid =
  getsGame
    $ map toId
    . filter (\o -> toCardCode o == toCardCode Objectives.overcomeByFear && (toAttrs o :: ObjectiveAttrs).placement == AttachedTo (PlayerTarget pid))
    . toList
    . entitiesObjectives
    . gameEntities

-- | The set-aside copy the player may attach: none while his dial has one.
attachableOvercomeByFear :: HasGame m => PlayerId -> m (Maybe EncounterCard)
attachableOvercomeByFear pid = do
  attached <- overcomeByFearOn pid
  setAside' <- getSetAside (CardWithCode $ toCardCode Objectives.overcomeByFear)
  pure $ if null attached then headMay setAside' else Nothing

{- | "Either attach a set aside Overcome by Fear to your threat dial, or ...": the player
chooses, the other way taken alone when no copy can be attached, there being none set
aside or one on his dial already. With neither possible, nothing happens.
-}
attachOvercomeByFearOr
  :: (ReverseQueue m, Targetable source)
  => source
  -> PlayerId
  -> Text
  -> Bool
  -> QueueT Message m ()
  -> m ()
attachOvercomeByFearOr source pid key otherwise' other = do
  card <- attachableOvercomeByFear pid
  chooseOrRunOneM pid do
    questionLabeled (key <> ".choose")
    causedBy source
    for_ card \c -> labeled "overcomeByFear.attach" $ putObjectiveIntoPlayAttached c (PlayerTarget pid)
    when otherwise' $ labeled (key <> ".other") other

-- | "Either attach a set aside Overcome by Fear to your threat dial, or discard an ally you
-- control."
attachOvercomeByFearOrDiscardAlly :: (ReverseQueue m, Targetable source) => source -> PlayerId -> Text -> m ()
attachOvercomeByFearOrDiscardAlly source pid key = do
  allies <- select (AllyControlledBy $ PlayerWithId pid)
  attachOvercomeByFearOr source pid key (notNull allies) do
    chooseOrRunOneM pid do
      questionLabeled "overcomeByFear.discardAlly"
      causedBy source
      targets allies discard

-- | "A player with threat 35 or higher", as Haunted Path and Blood Runs Chill read.
hasThreatAtLeast35 :: HasGame m => PlayerId -> m Bool
hasThreatAtLeast35 pid = (>= 35) . (.threat) <$> getPlayer pid

-- | Stage 3B, The Dead are Following, with its progress, while it is in play.
theDeadAreFollowing :: HasGame m => m (Maybe (QuestId, Int))
theDeadAreFollowing =
  getsGame
    $ headMay
    . map (\q -> (toId q, q.progress))
    . filter (\q -> toCardCode q == toCardCode Quests.theDeadAreFollowing && (toAttrs q).flipped)
    . toList
    . entitiesQuests
    . gameEntities

-- | An Oathbreaker enemy card, as The Stone of Erech and Trysting Place search for.
oathbreakerEnemy :: CardMatcher
oathbreakerEnemy = CardMatchAll [CardWithType EnemyType, CardWithTrait Oathbreaker]

{- | "The first player may raise each player's threat by 1 to search his deck for Andúril
and add it to his hand", as the campaign cards of The Flame of the West's scenarios read
in their Setup. Nothing is asked when his deck holds no Andúril.
-}
mayRaiseThreatToSearchForAnduril :: (ReverseQueue m, Sourceable source) => source -> m ()
mayRaiseThreatToSearchForAnduril source = do
  lead <- getLead
  deck <- (.deck) <$> getPlayer lead
  when (any (cardMatches anduril) deck) do
    chooseOneM lead do
      questionLabeled "campaign.anduril"
      labeled "campaign.searchForAnduril" do
        eachPlayer (`raiseThreat` 1)
        searchDeck lead source maxBound anduril False
      labeled "pass" $ pure ()
 where
  anduril = CardWithTitle "Andúril"
