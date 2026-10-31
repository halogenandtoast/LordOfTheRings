module LordOfTheRings.Scenario.Scenarios.RaidOnTheGreyHavens (raidOnTheGreyHavens) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithCardCode), select)
import LordOfTheRings.Modifier (ModifierType (CannotHaveAttachments))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DreamChaserCampaign (runDreamChaserCampaign)

newtype RaidOnTheGreyHavens = RaidOnTheGreyHavens ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

raidOnTheGreyHavens :: Scenario
raidOnTheGreyHavens =
  Scenario
    $ scenario (RaidOnTheGreyHavens . (nightmareDeckL ?~ nightmare)) "raid-on-the-grey-havens" "Raid on the Grey Havens"
 where
  -- Sahír's Advance is replaced by the Nightmare Deck's stage 2
  nightmare =
    nightmareDeck
      Nightmare.raidOnTheGreyHavensNightmare
      EncounterSet.RaidOnTheGreyHavensNightmare
      [(Enemies.corsairArsonist, 3), (Locations.elvenWaveRunner, 2), (Enemies.umbarRaider, 1)]

-- | Nightmare: "Captain Sahír and Na'asiyah each gain: 'Cannot have attachments.'" Its
-- setup instruction is the first stage's: "LordOfTheRings.Quest.Cards.TheCorsairsAssault".
instance HasModifiersFor RaidOnTheGreyHavens where
  getModifiersFor (RaidOnTheGreyHavens a) = when a.nightmare do
    elite <- concatMapM (select . EnemyWithCardCode . toCardCode) [Enemies.captainSahir, Enemies.naasiyah]
    for_ elite \e -> modified a e [CannotHaveAttachments]

{- | The encounter deck is built from the Raid on the Grey Havens and Corsair Raiders sets.
Captain Sahír and Na'asiyah of The Stormcaller Elite are set aside, enemy side faceup, for
Sahír's Advance to add to the staging area. There is no Corsair deck and no fleet: the
Dream-chaser is a location here.
-}
instance RunMessage RaidOnTheGreyHavens where
  runMessage msg (RaidOnTheGreyHavens attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <- gatherEncounterSets attrs [EncounterSet.RaidOnTheGreyHavens, EncounterSet.CorsairRaiders]
      elite <- traverse genEncounterCard [Enemies.captainSahir, Enemies.naasiyah]
      firstStage <- genEncounterCard Quests.theCorsairsAssault
      laterStages <- traverse genEncounterCard [nightmareOr attrs Quests.sahirsAdvance Quests.sahirsAdvanceNightmare]
      putQuestIntoPlay firstStage
      pure
        $ RaidOnTheGreyHavens
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ elite
    -- Campaign: the Dream-chaser campaign card's setup, Forced effects and resolution
    _ | isJust attrs.campaign -> RaidOnTheGreyHavens <$> runDreamChaserCampaign msg attrs
    _ -> RaidOnTheGreyHavens <$> runMessage msg attrs
