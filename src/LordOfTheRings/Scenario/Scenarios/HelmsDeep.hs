module LordOfTheRings.Scenario.Scenarios.HelmsDeep (helmsDeep) where

import LordOfTheRings.Campaign (CampaignLogUpdate (AddToPool), PoolCard (..), updateLog)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getRemainingPlayers)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype HelmsDeep = HelmsDeep ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

helmsDeep :: Scenario
helmsDeep = Scenario $ scenario HelmsDeep "helms-deep" "Helm's Deep"

{- | Helm's Deep and Orcs of the White Hand make up the encounter deck (MEC45 rules). Stage
1A's setup sets aside The Defense of Helm's Deep, 1 copy of Deeping Wall and, for each
player, 1 copy of Soldier of Isengard: the scenario does it as it gathers. Poisoned
Counsels is not gathered, having a player card back.

Campaign: Setup: Each player may change hero cards without the +1 threat penalty (see
'LordOfTheRings.Campaign.freeHeroChange'), and the burdens of the Shadow of the Past,
Flight to the Ford and The Ring Goes South burden sets are left out (see
'LordOfTheRings.Saga.removedBurdenSets').
-}
instance RunMessage HelmsDeep where
  runMessage msg (HelmsDeep attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.HelmsDeep, EncounterSet.OrcsOfTheWhiteHand]
      players <- length <$> getRemainingPlayers
      let copies n def = take n (filter ((== toCardCode def) . toCardCode) cards)
          aside = copies 1 Objectives.theDefenseOfHelmsDeep <> copies 1 Locations.deepingWall <> copies players Enemies.soldierOfIsengard
      firstStage <- genEncounterCard Quests.theKingOfTheGoldenHall
      rest <- traverse genEncounterCard [Quests.assaultOnHelmsDeep, Quests.theHostsOfIsengard, Quests.theWallIsBreached, Quests.theKingRidesOut]
      putQuestIntoPlay firstStage
      pure
        $ HelmsDeep
        $ attrs
        & encounterDeckL .~ filter (`notElem` aside) cards
        & setAsideL .~ aside
        & questDeckL .~ rest
    -- Resolution: Each player with a copy of Poisoned Counsels in his hand, deck or discard
    -- pile has earned that burden. Add the earned cards to the campaign pool: each for his
    -- seat, shuffled into his deck in the scenarios to come.
    ResolveCampaign | Just l <- attrs.campaign -> do
      order <- getsGame (.playerOrder)
      players <- getsGame (.players)
      let holds p = any isCounsels (p.hand <> p.deck <> p.discard)
          isCounsels = \case
            EncounterCard c -> toCardCode c == toCardCode Treacheries.poisonedCounselsTheTreasonOfSaruman
            _ -> False
          earned = [seat | (seat, pid) <- zip [0 ..] order, Just p <- [lookup pid players], holds p]
          counsels seat = AddToPool $ PoolCard (toCardCode Treacheries.poisonedCounselsTheTreasonOfSaruman) (Just seat) Nothing
      HelmsDeep <$> runMessage msg (attrs & campaignL ?~ foldl' (flip $ updateLog . counsels) l earned)
    _ -> HelmsDeep <$> runMessage msg attrs
