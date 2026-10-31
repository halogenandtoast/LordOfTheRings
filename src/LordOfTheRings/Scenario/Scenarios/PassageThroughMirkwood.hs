module LordOfTheRings.Scenario.Scenarios.PassageThroughMirkwood (passageThroughMirkwood) where

import LordOfTheRings.Campaign
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Matcher (getLead, getPlayerOrder)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MirkwoodPaths (putMendorIntoPlay)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype PassageThroughMirkwood = PassageThroughMirkwood ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

passageThroughMirkwood :: Scenario
passageThroughMirkwood =
  Scenario
    $ scenario (PassageThroughMirkwood . (nightmareDeckL ?~ nightmare)) "passage-through-mirkwood" "Passage Through Mirkwood"
 where
  nightmare =
    nightmareDeck
      Nightmare.passageThroughMirkwoodNightmare
      EncounterSet.PassageThroughMirkwoodNightmare
      [ (Enemies.dolGuldurOrcs, 3)
      , (Enemies.blackForestBats, 1)
      , (Locations.forestGate, 2)
      , (Enemies.ungoliantsSpawn, 1)
      , (Locations.mountainsOfMirkwood, 3)
      , (Treacheries.caughtInAWeb, 2)
      , (Enemies.forestSpider, 3)
      , (Locations.oldForestRoad, 1)
      ]

instance RunMessage PassageThroughMirkwood where
  runMessage msg (PassageThroughMirkwood attrs) = case msg of
    -- the first stage is out from the start, its setup followed at step 7 as ever
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.DolGuldurOrcs, EncounterSet.PassageThroughMirkwood, EncounterSet.SpidersOfMirkwood]
      firstStage <- genEncounterCard Quests.fliesAndSpiders
      laterStages <-
        traverse
          genEncounterCard
          [Quests.aForkInTheRoad, Quests.aChosenPathDontLeaveThePath, Quests.aChosenPathBeornsPath]
      putQuestIntoPlay firstStage
      pure $ PassageThroughMirkwood $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Nightmare: "Forced: During setup, each player reveals 1 card from the encounter deck
    -- and adds it to the staging area."
    DoStep 8 Setup | attrs.nightmare -> do
      eachPlayer \_ -> revealEncounterCard
      pure $ PassageThroughMirkwood attrs
    -- A Fork in the Road: proceed to one of the 2 A Chosen Path stages, at random.
    AdvanceQuest | stages@[_, _] <- attrs.questDeck, all isChosenPath stages -> do
      push . AdvanceToQuest =<< uniform stages
      pure $ PassageThroughMirkwood $ attrs & questDeckL .~ []
    -- The stage comes out on its first side, which both paths share, and the lead turns
    -- it over to see which path it is; the campaign card's resolution reads which.
    AdvanceToQuest card | isChosenPath card -> do
      putQuestIntoPlay card
      lead <- getLead
      chooseOneM lead do
        questionLabeledWith "revealPath" []
        targeting card $ push $ Flip (toTarget card)
      pure $ PassageThroughMirkwood $ attrs {scenarioMeta = toJSON (toCardCode card)}
    -- Campaign: Setup: Put Mendor into play, under the first player, chosen by now.
    DoStep 4 Setup | isJust attrs.campaign -> do
      putMendorIntoPlay
      PassageThroughMirkwood <$> runMessage msg attrs
    -- Resolution: Each player earns 1 copy of the Mendor's Support boon card. If Stage 3B
    -- was Beorn's Path, add the Ungoliant's Swarm burden card to the Campaign Pool; if it
    -- was Don't Leave the Path, the Lingering Venom burden card.
    ResolveCampaign | Just l <- attrs.campaign -> do
      seats <- length <$> getPlayerOrder
      let support = [PoolCard (toCardCode Events.mendorsSupport) (Just seat) Nothing | seat <- [0 .. seats - 1]]
          burden
            | attrs.meta == toJSON (toCardCode Quests.aChosenPathBeornsPath) = [Enemies.ungoliantsSwarm]
            | attrs.meta == toJSON (toCardCode Quests.aChosenPathDontLeaveThePath) = [Treacheries.lingeringVenom]
            | otherwise = []
          l' = foldl' (flip addToPool) l (support <> [PoolCard (toCardCode b) Nothing Nothing | b <- burden])
      PassageThroughMirkwood <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    _ -> PassageThroughMirkwood <$> runMessage msg attrs
   where
    isChosenPath = (`elem` map toCardCode [Quests.aChosenPathDontLeaveThePath, Quests.aChosenPathBeornsPath]) . toCardCode
