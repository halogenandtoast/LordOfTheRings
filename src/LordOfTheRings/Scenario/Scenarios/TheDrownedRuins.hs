module LordOfTheRings.Scenario.Scenarios.TheDrownedRuins (theDrownedRuins) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (CardWithTrait), cardIs, cardMatches)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Trait (Trait (Grotto))
import LordOfTheRings.Scenario.Scenarios.DreamChaserCampaign (runDreamChaserCampaign)

newtype TheDrownedRuins = TheDrownedRuins ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDrownedRuins :: Scenario
theDrownedRuins = Scenario $ scenario TheDrownedRuins "the-drowned-ruins" "The Drowned Ruins"

{- | The encounter deck is built from The Drowned Ruins and Drowned Dead; Captain Sahír and
Na'asiyah come from The Stormcaller Elite, objective-ally sides faceup, as the first stage
is set up. Shrine to Morgoth is set aside, and the other Grotto locations, shuffled, are
the Grotto deck (The Drowned Ruins rules insert); Drowned Cave, single-sided, stays in the
encounter deck.
-}
instance RunMessage TheDrownedRuins where
  runMessage msg (TheDrownedRuins attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- concatMapM (gatherEncounterSet attrs) [EncounterSet.TheDrownedRuins, EncounterSet.DrownedDead]
      let (grotto, encounterDeck) = partition (cardMatches $ CardWithTrait Grotto) cards
          (shrine, grottoDeck) = partition (cardMatches $ cardIs Locations.shrineToMorgothTheDrownedRuins) grotto
      shuffled <- shuffleM grottoDeck
      firstStage <- genEncounterCard Quests.theGrotto
      laterStages <- traverse genEncounterCard [Quests.sahirsBetrayal]
      putQuestIntoPlay firstStage
      pure
        $ TheDrownedRuins
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ shrine
        & grottoDeckL .~ shuffled
    -- Campaign: the Dream-chaser campaign card's setup, Forced effects and resolution
    _ | isJust attrs.campaign -> TheDrownedRuins <$> runDreamChaserCampaign msg attrs
    _ -> TheDrownedRuins <$> runMessage msg attrs
