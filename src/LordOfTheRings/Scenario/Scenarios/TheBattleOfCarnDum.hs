module LordOfTheRings.Scenario.Scenarios.TheBattleOfCarnDum (theBattleOfCarnDum) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogKey (HeroDamage), recordCount)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Entities (entitiesHeroes)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (angmarSetup, chooseBoonForHand, thaurdirsBurdens)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheBattleOfCarnDum = TheBattleOfCarnDum ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBattleOfCarnDum :: Scenario
theBattleOfCarnDum =
  Scenario
    $ scenario (TheBattleOfCarnDum . (nightmareDeckL ?~ nightmare)) "the-battle-of-carn-dum" "The Battle of Carn Dûm"
 where
  -- its setup card only explains the Siege keyword, Protect the Flanks'
  nightmare =
    nightmareDeck
      Nightmare.theBattleOfCarnDumNightmare
      EncounterSet.TheBattleOfCarnDumNightmare
      [ (Treacheries.vileAffliction, 1)
      , (Quests.furiousCharge, 1)
      , (Quests.orcAmbush, 1)
      , (Enemies.angmarOrc, 1)
      , (Enemies.angmarMarauder, 3)
      , (Treacheries.terrorOfTheNorth, 3)
      , (Locations.accursedBattlefield, 2)
      , (Locations.blightOfCarnDum, 1)
      ]

-- | The encounter deck is built from The Battle of Carn Dûm, Angmar Orcs and Dark Sorcery.
instance RunMessage TheBattleOfCarnDum where
  runMessage msg (TheBattleOfCarnDum attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheBattleOfCarnDum, EncounterSet.AngmarOrcs, EncounterSet.DarkSorcery]
      firstStage <- genEncounterCard Quests.theClutchesOfCarnDum
      laterStages <- traverse genEncounterCard [Quests.midwintersCrux]
      putQuestIntoPlay firstStage
      pure $ TheBattleOfCarnDum $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Campaign: Setup: Each player may change hero cards without incurring the +1 threat
    -- penalty, and Amarthiúl may be included among a player's heroes (see
    -- 'freeHeroChange' and 'lockedHeroes'). Then, each player in turn order may select one
    -- of the following boons and add it to their opening hand: Elrond's Champion, Lore of
    -- Ancient Arnor, Raiment of the Second Age, or Rivendell Skirmishers. Add the chosen
    -- cards to the Campaign Pool.
    DoStep 9 Setup | isJust attrs.campaign -> do
      angmarSetup attrs
      thaurdirsBurdens attrs False
      chooseBoonForHand [Allies.elrondsChampion, Events.loreOfAncientArnor, Attachments.raimentOfTheSecondAge, Allies.rivendellSkirmishers]
      TheBattleOfCarnDum <$> runMessage msg attrs
    CheckWindows _ | isJust attrs.campaign -> do
      thaurdirsBurdens attrs False
      TheBattleOfCarnDum <$> runMessage msg attrs
    -- Resolution: Note the amount of damage on each hero at the end of the game.
    ResolveCampaign | Just l <- attrs.campaign -> do
      heroes <- getsGame (toList . entitiesHeroes . gameEntities)
      let l' = foldl' (\l'' h -> recordCount HeroDamage (toCardCode h) h.damage l'') l heroes
      TheBattleOfCarnDum <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    _ -> TheBattleOfCarnDum <$> runMessage msg attrs
