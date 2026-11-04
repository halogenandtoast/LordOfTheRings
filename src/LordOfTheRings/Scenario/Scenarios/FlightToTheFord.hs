module LordOfTheRings.Scenario.Scenarios.FlightToTheFord (flightToTheFord) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Ability (anyPlayer, response, restricted)
import LordOfTheRings.Campaign (CampaignLogUpdate (AddToPool), PoolCard (..), updateLog)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (cardIs, cardMatches)
import LordOfTheRings.Criteria (Criterion (BurdenDeckNotEmpty))
import LordOfTheRings.Matcher.Window (WindowMatcher (EnemyDeclaredAsAttackerWindow))
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (ringBearersLifeOf)
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Trait (Trait (Burden))
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (EnemyDeclaredAsAttacker))

newtype FlightToTheFord = FlightToTheFord ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

flightToTheFord :: Scenario
flightToTheFord = Scenario $ scenario FlightToTheFord "flight-to-the-ford" "Flight to the Ford"

-- | Campaign: Response: After an enemy is declared as an attacker, shuffle the top card of
-- the burden deck into the encounter deck to cancel that attack. (Any player may trigger
-- that ability.)
instance HasAbilities FlightToTheFord where
  getAbilities (FlightToTheFord a) =
    [anyPlayer $ restricted a 1 BurdenDeckNotEmpty $ response $ EnemyDeclaredAsAttackerWindow After | isJust a.campaign]

{- | Flight to the Ford, Hunted, The Black Riders and The Nazgûl make up the encounter deck
(The Black Riders rules insert). Stage 1A sets Ford of Bruinen and The Witch-king aside,
which the scenario does as it gathers; the stage builds the Burden deck and sets the
Ring-bearer's life, which the scenario keeps on its life tracker: "If the Ring-bearer's
life reaches 0, the players lose the game" (rules insert, "The Ring-bearer's Life").
-}
instance RunMessage FlightToTheFord where
  runMessage msg s@(FlightToTheFord attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.FlightToTheFord, EncounterSet.Hunted, EncounterSet.TheBlackRiders, EncounterSet.TheNazgul]
      let aside c = any (`cardMatches` c) [cardIs Locations.fordOfBruinen, cardIs Enemies.theWitchKing]
          (setAside', deck) = partition aside cards
      putQuestIntoPlay =<< genEncounterCard Quests.pursuedByTheEnemy
      laterStages <- traverse genEncounterCard [Quests.raceToRivendell]
      pure
        $ FlightToTheFord
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ setAside'
        & questDeckL .~ laterStages
    ScenarioSpecific "flightToTheFord.setLife" (parseMaybe parseJSON -> Just (n :: Int)) ->
      pure $ FlightToTheFord $ attrs & metaL .~ object ["ringBearersLife" .= n]
    ScenarioSpecific "flightToTheFord.reduceLife" (parseMaybe parseJSON -> Just (n :: Int)) -> do
      let life = max 0 (ringBearersLifeOf attrs.meta - n)
      when (life == 0) $ gameOver Lost
      pure $ FlightToTheFord $ attrs & metaL .~ object ["ringBearersLife" .= life]
    UseCardAbility _ ScenarioSource 1 ws -> do
      shuffleTopOfBurdenDeckIntoEncounterDeck
      for_ [eid | Window _ (EnemyDeclaredAsAttacker eid _) <- ws] cancelAttack
      pure s
    -- Resolution: Add each burden card in play, in the encounter deck, and in the discard
    -- pile to the Campaign Pool. Then added cards have been earned by the players. A
    -- burden the pool holds already, earned in an earlier scenario, stays there once.
    ResolveCampaign | Just l <- attrs.campaign -> do
      treacheries <- getsGame (map toCardDef . toList . (.entities.treacheries))
      objectives <- getsGame (map toCardDef . toList . (.entities.objectives))
      let found = treacheries <> objectives <> map toCardDef attrs.encounterDeck <> map toCardDef attrs.encounterDiscard
          pooled = map (.code) l.pool
          earned = ordNub [toCardCode d | d <- found, Burden `member` cdCardTraits d, toCardCode d `notElem` pooled]
          l' = foldl' (\lg code -> updateLog (AddToPool $ PoolCard code Nothing Nothing) lg) l earned
      FlightToTheFord <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    _ -> FlightToTheFord <$> runMessage msg attrs
