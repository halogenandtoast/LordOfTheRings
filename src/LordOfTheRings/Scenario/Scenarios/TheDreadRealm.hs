module LordOfTheRings.Scenario.Scenarios.TheDreadRealm (theDreadRealm) where

import LordOfTheRings.Campaign (CampaignLogKey (HeroDamage), recordedCount)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (entitiesHeroes)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Matcher (getPlayer)
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (angmarSetup)
import LordOfTheRings.Trait (Trait (Boon))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheDreadRealm = TheDreadRealm ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDreadRealm :: Scenario
theDreadRealm = Scenario $ scenario TheDreadRealm "the-dread-realm" "The Dread Realm"

{- | The encounter deck is built from The Dread Realm, Cursed Dead and Dark Sorcery.
Setup (The Catacombs of Carn Dûm): Set Daechanar and Altar of Midwinter aside, out of
play; the stage itself reveals and reanimates.
-}
instance RunMessage TheDreadRealm where
  runMessage msg (TheDreadRealm attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- concatMapM (gatherEncounterSet attrs) [EncounterSet.TheDreadRealm, EncounterSet.CursedDead, EncounterSet.DarkSorcery]
      let (aside, encounterDeck) = partition ((`elem` map toCardCode [Enemies.daechanar, Locations.altarOfMidwinter]) . toCardCode) cards
      firstStage <- genEncounterCard Quests.theCatacombsOfCarnDum
      laterStages <- traverse genEncounterCard [Quests.angmarAwakened, Quests.daechanarsFall]
      putQuestIntoPlay firstStage
      pure $ TheDreadRealm $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages & setAsideL .~ aside
    -- Campaign: Setup: Each player may search their deck for a boon card and add it to their
    -- opening hand. Each hero begins the game with damage on them equal to the amount noted
    -- for them in the Campaign Log.
    -- Resolution: The players win the campaign!
    DoStep 9 Setup | Just l <- attrs.campaign -> do
      angmarSetup attrs
      eachPlayer \p -> do
        deck <- (.deck) <$> getPlayer p
        chooseOneM p do
          questionLabeled "campaign.searchForBoon"
          labeled "pass" $ pure ()
          for_ [c | c <- deck, Boon `member` cdCardTraits (toCardDef c)] \c ->
            targeting (CardIdTarget c.id) $ takeFromDeck p c
        shufflePlayerDeck p
      heroes <- getsGame (toList . entitiesHeroes . gameEntities)
      for_ heroes \h -> do
        let noted = recordedCount HeroDamage (toCardCode h) l
        when (noted > 0) $ placeDamage attrs h.id noted
      TheDreadRealm <$> runMessage msg attrs
    _ -> TheDreadRealm <$> runMessage msg attrs
