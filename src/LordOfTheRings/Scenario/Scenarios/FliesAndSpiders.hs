module LordOfTheRings.Scenario.Scenarios.FliesAndSpiders (fliesAndSpiders) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (PoolCard (..), addToPool)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Game.Base (gamePlayerOrder)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype FliesAndSpiders = FliesAndSpiders ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fliesAndSpiders :: Scenario
fliesAndSpiders = Scenario $ scenario FliesAndSpiders "flies-and-spiders" "Flies and Spiders"

{- | Flies and Spiders and Wilderland make up the encounter deck (The Hobbit: On the
Doorstep rules insert). Stage 1A sets The Spider's Glade aside, out of play, until stage 4A
adds it to the staging area. The first player takes control of Bilbo Baggins as the game
sets up (see 'LordOfTheRings.Saga.fellowshipSetup').
-}
instance RunMessage FliesAndSpiders where
  runMessage msg (FliesAndSpiders attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.FliesAndSpiders, EncounterSet.Wilderland]
      let (glade, deck) = partition ((== toCardCode Locations.theSpidersGlade) . toCardCode) cards
      firstStage <- genEncounterCard Quests.intoMirkwood
      laterStages <- traverse genEncounterCard [Quests.lostInTheDarkFliesAndSpiders, Quests.capturedBySpiders, Quests.battleWithTheSpiders]
      putQuestIntoPlay firstStage
      pure
        $ FliesAndSpiders
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ glade
        & questDeckL .~ laterStages
    -- Won, the players have discovered the treasure card Bilbo's Magic Ring: each of them
    -- may add it to his deck in the scenarios that list its set (The Hobbit: On the
    -- Doorstep rules insert, "Treasure").
    ResolveCampaign | Just l <- attrs.campaign -> do
      seats <- getsGame (length . gamePlayerOrder)
      let ring seat = PoolCard {poolCardCode = toCardCode Attachments.bilbosMagicRing, poolCardSeat = Just seat, poolCardAttachedTo = Nothing}
          l' = foldl' (flip addToPool) l (map ring [0 .. seats - 1])
      FliesAndSpiders <$> runMessage msg (attrs & campaignL ?~ l')
    _ -> FliesAndSpiders <$> runMessage msg attrs
