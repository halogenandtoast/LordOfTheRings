module LordOfTheRings.Scenario.Scenarios.TheBlackGateOpens (theBlackGateOpens) where

import LordOfTheRings.Ally.Types (AllyAttrs (allyPlacement))
import LordOfTheRings.Campaign (CampaignLog (campaignLogPool), CampaignLogKey (BlackGateResources), CampaignLogUpdate (RemoveFallenHero), PoolCard (..), addToPool, recordCount, updateLog)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getLead)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBlackGateOpens.Helpers (blackGateResources)

newtype TheBlackGateOpens = TheBlackGateOpens ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBlackGateOpens :: Scenario
theBlackGateOpens = Scenario $ scenario TheBlackGateOpens "the-black-gate-opens" "The Black Gate Opens"

-- | Wraith on Wings, a burden The Battle of the Pelennor Fields' resolution adds to the
-- Campaign Pool, a copy for each player.
wraithOnWings :: CardCode
wraithOnWings = toCardCode Enemies.wraithOnWings

{- | The Black Gate Opens and Orcs of Mordor make up the encounter deck (The Mountain of
Fire rules insert). The Black Gate, The Mouth of Sauron and Gwaihir are set aside for
stage 1A. The first player takes control of the Fellowship sphere Aragorn as the game sets
up (see 'LordOfTheRings.Saga.fellowshipSetup').

The scenario cannot be won: stage 2B cannot be defeated. In campaign mode its campaign
card resolves once the players lose (see 'LordOfTheRings.Saga.campaignResolvesOn').
-}
instance RunMessage TheBlackGateOpens where
  runMessage msg (TheBlackGateOpens attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheBlackGateOpens, EncounterSet.OrcsOfMordor]
      let aside c = toCardCode c `elem` map toCardCode [Locations.theBlackGate, Enemies.theMouthOfSauron, Objectives.gwaihirTheBlackGateOpens]
          (setAside', deck) = partition aside cards
      firstStage <- genEncounterCard Quests.embassyFromTheDarkTower
      laterStages <- traverse genEncounterCard [Quests.theBattleOfTheMorannon]
      putQuestIntoPlay firstStage
      pure
        $ TheBlackGateOpens
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ setAside'
        & questDeckL .~ laterStages
    -- Campaign: Setup: Each player may change heroes he controls without incurring the +1
    -- threat penalty (see 'LordOfTheRings.Campaign.freeHeroChange'). The first player may
    -- raise each player's threat by 1 to search his deck for Andúril and add it to his
    -- hand. Remove each burden with the following burden set icons from the encounter
    -- deck: ASotP, FtF, TRGS, TPofM, SL (see 'LordOfTheRings.Saga.removedBurdenSets').
    -- The search comes as the quest is set up, before stage 1B's When Revealed turns over
    -- the top cards of the players' decks.
    DoStep 7 Setup | isJust attrs.campaign -> do
      mayRaiseThreatToSearchForAnduril attrs
      TheBlackGateOpens <$> runMessage msg attrs
    -- Resolution: Record the number of resource tokens on The Black Gate in the notes
    -- section of the campaign log. If a Wraith on Wings is in the victory display, remove
    -- that copy from the campaign pool. If the first player controls Gwaihir, add him to the
    -- campaign pool. The players have earned that boon. If a hero is in its controller's
    -- discard pile, do not add that hero's name to the list of fallen heroes: the list is
    -- read once every campaign's resolution has added to it.
    ResolveCampaign | Just l <- attrs.campaign -> do
      resources <- blackGateResources
      lead <- getLead
      gwaihir <-
        getsGame
          $ any (\a -> toCardCode a == toCardCode Objectives.gwaihirTheBlackGateOpens && allyPlacement (toAttrs a) == InPlayArea lead)
          . toList
          . (.entities.allies)
      players <- getsGame (toList . (.players))
      let wraiths = length [() | c <- attrs.victoryDisplay, toCardCode c == wraithOnWings]
          removeOne pool = let (before, after) = break ((== wraithOnWings) . (.code)) pool in before <> drop 1 after
          l' =
            (if gwaihir then addToPool (PoolCard (toCardCode Objectives.gwaihirTheBlackGateOpens) Nothing Nothing) else id)
              $ recordCount BlackGateResources (toCardCode Locations.theBlackGate) resources
              $ l {campaignLogPool = foldl' (\pool _ -> removeOne pool) l.pool [1 .. wraiths]}
          discarded = [toCardCode c | p <- players, c <- p.discard, cdCardType (toCardDef c) == HeroType]
      attrs' <- runMessage msg (attrs {scenarioCampaign = Just l'})
      pure $ TheBlackGateOpens $ attrs' {scenarioCampaign = (\lg -> foldl' (flip updateLog) lg (map RemoveFallenHero discarded)) <$> attrs'.campaign}
    _ -> TheBlackGateOpens <$> runMessage msg attrs
