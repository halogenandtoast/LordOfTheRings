module LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard (theRoadToIsengard) where

import Data.Aeson.Types (parseMaybe)
import Data.List (elemIndex)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogUpdate (AddToPool), PoolCard (..), updateLog)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Matcher (cardIs, cardMatches, getLead, getPlayer, getRemainingPlayers)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRoadToIsengard.Helpers (canceledOf, progressOf)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheRoadToIsengard = TheRoadToIsengard ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRoadToIsengard :: Scenario
theRoadToIsengard = Scenario $ scenario TheRoadToIsengard "the-road-to-isengard" "The Road to Isengard"

{- | The Road to Isengard and Snaga Orcs make up the encounter deck (MEC45 rules). Stage 1A
sets Gates of Isengard, Orthanc, Saruman and Saruman's Staff aside and adds each Ent of
Fangorn to the staging area: all of them are kept out of the deck here, for the stages to
take them from there.

The scenario keeps the progress placed on the quest this phase in its meta, for stage 3B
to cancel as Orthanc is explored (see the stage).
-}
instance RunMessage TheRoadToIsengard where
  runMessage msg (TheRoadToIsengard attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheRoadToIsengard, EncounterSet.SnagaOrcs]
      let aside c =
            any
              (`cardMatches` c)
              [ cardIs Locations.gatesOfIsengard
              , cardIs Locations.orthanc
              , cardIs Enemies.sarumanTheRoadToIsengard
              , cardIs Objectives.sarumansStaffTheRoadToIsengard
              , cardIs Objectives.entOfFangorn
              ]
          (setAside', deck) = partition aside cards
      firstStage <- genEncounterCard Quests.rouseTheEnts
      laterStages <- traverse genEncounterCard [Quests.breakingIsengard, Quests.theVoiceOfSaruman]
      putQuestIntoPlay firstStage
      pure
        $ TheRoadToIsengard
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ setAside'
        & questDeckL .~ laterStages
    -- Campaign: Setup: Shuffle Saruman's Voice into the encounter deck. (Each burden of the
    -- Shadow of the Past, Flight to the Ford and The Ring Goes South burden sets is left out
    -- of the encounter deck: see 'LordOfTheRings.Saga.removedBurdenSets'.)
    DoStep 9 Setup | isJust attrs.campaign -> do
      voice <- genEncounterCard Treacheries.sarumansVoice
      deck <- shuffleM (voice : attrs.encounterDeck)
      TheRoadToIsengard <$> runMessage msg (attrs & encounterDeckL .~ deck)
    ScenarioSpecific "roadToIsengard.questProgress" (parseMaybe parseJSON -> Just (n :: Int)) ->
      pure $ TheRoadToIsengard $ attrs & metaL .~ meta (progressOf attrs.meta + n) (canceledOf attrs.meta)
    ScenarioSpecific "roadToIsengard.cancelQuestProgress" _ ->
      pure $ TheRoadToIsengard $ attrs & metaL .~ meta 0 True
    EndPhase _ -> TheRoadToIsengard <$> runMessage msg (attrs & metaL .~ meta 0 False)
    {- Resolution: If any player has 3 or fewer cards in his hand, the players have earned
    the Saruman's Voice burden card. If any player has 3 or more cards in his hand, the
    players have earned the Palantír of Orthanc boon card. Add the earned cards to the
    Campaign Pool. The hands are read as the game is won, the players still in it.

    The Palantír goes to the first player's deck: it attaches to Aragorn, whom the first
    player controls, and a choice for the group is the first player's to make.
    -}
    ResolveCampaign | Just l <- attrs.campaign -> do
      players <- getRemainingPlayers
      hands <- traverse (fmap (length . (.hand)) . getPlayer) players
      lead <- getLead
      order <- getsGame (.playerOrder)
      let voice = [PoolCard (toCardCode Treacheries.sarumansVoice) Nothing Nothing | any (<= 3) hands]
          palantir = [PoolCard (toCardCode Attachments.palantirOfOrthanc) (elemIndex lead order) Nothing | any (>= 3) hands]
          l' = foldl' (flip $ updateLog . AddToPool) l (voice <> palantir)
      TheRoadToIsengard <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    _ -> TheRoadToIsengard <$> runMessage msg attrs
   where
    meta :: Int -> Bool -> Value
    meta n canceled = object ["questProgress" .= n, "questProgressCanceled" .= canceled]
