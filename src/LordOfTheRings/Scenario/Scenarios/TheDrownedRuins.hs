module LordOfTheRings.Scenario.Scenarios.TheDrownedRuins (theDrownedRuins) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (CardWithTrait), cardIs, cardMatches)
import LordOfTheRings.Modifier (ModifierType (CannotCancelEncounterEffects))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (underwaterActive)
import LordOfTheRings.Trait (Trait (Grotto))
import LordOfTheRings.Scenario.Scenarios.DreamChaserCampaign (runDreamChaserCampaign)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheDrownedRuins = TheDrownedRuins ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Nightmare: its stage 2A Forced is Sahír's Betrayal's, in nightmare mode.
theDrownedRuins :: Scenario
theDrownedRuins =
  Scenario
    $ scenario (TheDrownedRuins . (nightmareDeckL ?~ nightmare)) "the-drowned-ruins" "The Drowned Ruins"
 where
  nightmare =
    nightmareDeck
      Nightmare.theDrownedRuinsNightmare
      EncounterSet.TheDrownedRuinsNightmare
      [ (Locations.cursedCaverns, 1)
      , (Locations.underseaGrotto, 1)
      , (Enemies.throngsOfUnfaithful, 2)
      , (Enemies.soullessCadaver, 2)
      , (Locations.drownedCave, 2)
      , (Treacheries.intoTheAbyss, 3)
      , (Treacheries.tanglingAndGrasping, 1)
      , (Enemies.seaScorpion, 2)
      , (Enemies.caveEel, 2)
      ]

-- | Nightmare: "While the active location is Underwater, encounter card effects cannot be
-- cancelled."
instance HasModifiersFor TheDrownedRuins where
  getModifiersFor (TheDrownedRuins a) = when a.nightmare do
    whenM underwaterActive $ modified a GameTarget [CannotCancelEncounterEffects]

{- | The encounter deck is built from The Drowned Ruins and Drowned Dead; Captain Sahír and
Na'asiyah come from The Stormcaller Elite, objective-ally sides faceup, as the first stage
is set up. Shrine to Morgoth is set aside, and the other Grotto locations, shuffled, are
the Grotto deck (The Drowned Ruins rules insert); Drowned Cave, single-sided, stays in the
encounter deck.
-}
instance RunMessage TheDrownedRuins where
  runMessage msg (TheDrownedRuins attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheDrownedRuins, EncounterSet.DrownedDead]
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
