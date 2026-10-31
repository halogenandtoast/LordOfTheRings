module LordOfTheRings.Scenario.Scenarios.EscapeFromDolGuldur (escapeFromDolGuldur) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Matcher (HeroMatcher (AnyHero), getHero, getLead, select)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo, InPlayArea))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Effect (Effect (effectSource))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MirkwoodPaths (mendorInPlay, putMendorIntoPlay)
import LordOfTheRings.Source (Source (CardCodeSource))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Window (..), WindowType (QuestedUnsuccessfully))

newtype EscapeFromDolGuldur = EscapeFromDolGuldur ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

escapeFromDolGuldur :: Scenario
escapeFromDolGuldur =
  Scenario
    $ scenario (EscapeFromDolGuldur . (nightmareDeckL ?~ nightmare)) "escape-from-dol-guldur" "Escape from Dol Guldur"
 where
  -- its setup card's Forced, as many prisoners as the players call for, is stage 1's
  nightmare =
    nightmareDeck
      Nightmare.escapeFromDolGuldurNightmare
      EncounterSet.EscapeFromDolGuldurNightmare
      [ (Enemies.dolGuldurOrcs, 2)
      , (Treacheries.theNecromancersReach, 1)
      , (Locations.necromancersPass, 1)
      , (Locations.enchantedStream, 2)
      , (Treacheries.underTheShadow, 2)
      , (Treacheries.ironShackles, 1)
      , (Locations.towerGate, 2)
      , (Locations.greatForestWeb, 1)
      , (Locations.mountainsOfMirkwood, 3)
      , (Treacheries.caughtInAWeb, 2)
      ]

instance RunMessage EscapeFromDolGuldur where
  runMessage msg (EscapeFromDolGuldur attrs) = case msg of
    -- the first stage is out from the start, its setup followed at step 7 as ever
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.EscapeFromDolGuldur, EncounterSet.SpidersOfMirkwood, EncounterSet.DolGuldurOrcs]
      firstStage <- genEncounterCard Quests.theNecromancersTower
      laterStages <- traverse genEncounterCard [Quests.throughTheCaverns, Quests.outOfTheDungeons]
      putQuestIntoPlay firstStage
      pure $ EscapeFromDolGuldur $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Nazgul of Dol Guldur: Forced: When the prisoner is "rescued", move Nazgul of Dol
    -- Guldur into the staging area.
    RescuePrisoner _ -> do
      let (nazgul, rest) = partition ((== toCardCode Enemies.nazgulOfDolGuldur) . toCardCode) attrs.scenarioSetAside
      traverse_ addToStaging nazgul
      pure $ EscapeFromDolGuldur $ attrs & setAsideL .~ rest
    -- Campaign: Setup: Put Mendor into play. Attach Appointed by Fate to a hero of the
    -- first player's choice.
    DoStep 4 Setup | isJust attrs.campaign -> do
      putMendorIntoPlay
      lead <- getLead
      heroes <- select AnyHero
      chooseOrRunOneM lead do
        questionLabeled "campaign.attachTo"
        targets heroes \h -> do
          owner <- (.owner) <$> getHero h
          (\card -> attachOutOfPlay owner card h) =<< genPlayerCard Attachments.appointedByFate (Just owner)
      EscapeFromDolGuldur <$> runMessage msg attrs
    -- Forced: (the prisoner taken) Mendor is captured as well. Place Mendor facedown next
    -- to the prisoner (this does not cause him to leave play). Flip this card over.
    TakePrisoner hid | isJust attrs.campaign -> do
      mendor <- mendorInPlay
      for_ mendor \m -> placeAlly m.id (AttachedTo $ toTarget hid)
      EscapeFromDolGuldur <$> runMessage msg (attrs {scenarioCampaignCardFlipped = True})
    -- Response: After the first objective is claimed, flip Mendor face-up and place 1
    -- damage token on him.
    ClaimObjective {} | isJust attrs.campaign -> do
      mendor <- mendorInPlay
      for_ mendor \m -> case m.placement of
        AttachedTo (HeroTarget _) -> do
          placeAlly m.id (InPlayArea m.owner)
          placeDamage attrs (AllyTarget m.id) 1
        _ -> pure ()
      EscapeFromDolGuldur <$> runMessage msg attrs
    -- Resolution: If Mendor is still in play, add Mendor to the Campaign Pool, to put into
    -- play at the start of each scenario of the next campaign (each player may include 1
    -- copy of Mendor's Support in their deck during it). If he is not, do not adjust it.
    ResolveCampaign | Just l <- attrs.campaign -> do
      mendor <- mendorInPlay
      let l' = if isJust mendor then record MendorJoinsTheNextCampaign [toCardCode Objectives.mendor] l else l
      EscapeFromDolGuldur <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    -- Sentinel of Shadow: "If the players quest unsuccessfully this phase, put the Nazgul of
    -- Dol Guldur into play (from any out of play area), engaged with the first player." Its
    -- staging bonus this phase marks it.
    CheckWindows ws | any ((== QuestedUnsuccessfully) . windowType) ws -> do
      sentinel <- getsGame (any ((== CardCodeSource (toCardCode Treacheries.sentinelOfShadow)) . effectSource) . gameEffects)
      let isNazgul = (== toCardCode Enemies.nazgulOfDolGuldur) . toCardCode
          outOfPlay = attrs.scenarioSetAside <> attrs.encounterDeck <> attrs.encounterDiscard
      case find isNazgul outOfPlay of
        Just nazgul | sentinel -> do
          lead <- getLead
          putEnemyIntoPlayEngaged lead nazgul
          pure
            $ EscapeFromDolGuldur
            $ attrs
            & (setAsideL %~ filter (/= nazgul))
            & (encounterDeckL %~ filter (/= nazgul))
            & (encounterDiscardL %~ filter (/= nazgul))
        _ -> EscapeFromDolGuldur <$> runMessage msg attrs
    _ -> EscapeFromDolGuldur <$> runMessage msg attrs
