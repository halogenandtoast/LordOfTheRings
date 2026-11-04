module LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur (theTreacheryOfRhudaur) where

import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogKey (BooksEarned), recordCount)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Entities (entitiesAllies, entitiesHeroes, entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo), placementController)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getMainStage)
import LordOfTheRings.Matcher.Types (CardMatcher (AnyCard))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (angmarSetup, thaurdirsBurdens, thaurdirsBurdensAtSetup)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Window (..), WindowType (AdvancedToStage))

newtype TheTreacheryOfRhudaur = TheTreacheryOfRhudaur ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTreacheryOfRhudaur :: Scenario
theTreacheryOfRhudaur =
  Scenario
    $ scenario (TheTreacheryOfRhudaur . (nightmareDeckL ?~ nightmare)) "the-treachery-of-rhudaur" "The Treachery of Rhudaur"
 where
  nightmare =
    nightmareDeck
      Nightmare.theTreacheryOfRhudaurNightmare
      EncounterSet.TheTreacheryOfRhudaurNightmare
      [ (Locations.decrepitRemains, 2)
      , (Locations.eerieHalls, 2)
      , (Treacheries.darkCovenant, 1)
      , (Locations.ruinsOfArnor, 2)
      , (Treacheries.tragicDiscovery, 2)
      , (Enemies.cursedDead, 5)
      ]

{- | The nightmare encounter sets of each side quest, by the Clue objective it flips to:
Quiet the Spirits' Apostates of Angmar, Sift through the Debris' Secret Antechambers and
Decipher Ancient Texts' Life Drains.
-}
nightmareSets :: [(CardDef, CardDef)]
nightmareSets =
  [ (Objectives.daechanarsBrand, Enemies.apostateOfAngmar)
  , (Objectives.heirloomOfIarchon, Locations.secretAntechamber)
  , (Objectives.ordersFromAngmar, Treacheries.lifeDrain)
  ]

{- | The Treachery of Rhudaur, Cursed Dead and Ruins of Arnor encounter sets.

Nightmare: "Setup: Set the Sift through the Debris, Decipher Ancient Texts, and Quiet the
Spirits encounter sets aside, out of play. Forced: After you advance to stage 2b, for each
of the above sidequests that was not defeated, shuffle the encounter set that matches its
title into the encounter deck, and shuffle the encounter discard pile into the encounter
deck." A side quest defeated has flipped to its Clue objective.
-}
instance RunMessage TheTreacheryOfRhudaur where
  runMessage msg (TheTreacheryOfRhudaur attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets attrs [EncounterSet.TheTreacheryOfRhudaur, EncounterSet.CursedDead, EncounterSet.RuinsOfArnor]
      let (aside, encounterDeck) = partition ((`elem` map (toCardCode . snd) nightmareSets) . toCardCode) cards
      firstStage <- genEncounterCard Quests.secretsOfRhudaur
      laterStages <- traverse genEncounterCard [Quests.thaurdirsPursuit]
      putQuestIntoPlay firstStage
      pure $ TheTreacheryOfRhudaur $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages & setAsideL <>~ aside
    CheckWindows ws | attrs.nightmare, AdvancedToStage `elem` map windowType ws -> do
      stage <- getMainStage
      when (stage == 2) do
        clues <- getsGame (map toCardCode . toList . entitiesObjectives . gameEntities)
        let undefeated = [toCardCode c | (clue, c) <- nightmareSets, toCardCode clue `notElem` clues]
        shuffleSetAsideIntoEncounterDeck [c | c <- attrs.scenarioSetAside, toCardCode c `elem` undefeated]
        shuffleEncounterDiscardIntoDeck AnyCard
      TheTreacheryOfRhudaur <$> runMessage msg attrs
    -- Campaign: what the pool brings, Thaurdir's burden on Thaurdir among it
    DoStep 9 Setup | isJust attrs.campaign -> do
      angmarSetup attrs
      thaurdirsBurdensAtSetup attrs
      TheTreacheryOfRhudaur <$> runMessage msg attrs
    CheckWindows _ | isJust attrs.campaign -> do
      thaurdirsBurdens attrs False
      TheTreacheryOfRhudaur <$> runMessage msg attrs
    -- Resolution: For each Clue a player controls, that player may add the book with the
    -- same name to the Campaign Pool and their deck.
    -- The game is over as this resolves, so the books are offered as the next scenario is
    -- set up ('angmarSetup').
    ResolveCampaign | Just l <- attrs.campaign -> do
      objectives <- getsGame (toList . entitiesObjectives . gameEntities)
      heroes <- getsGame (entitiesHeroes . gameEntities)
      allies <- getsGame (entitiesAllies . gameEntities)
      order <- getsGame (.playerOrder)
      let books =
            [ (Objectives.daechanarsBrand, Attachments.daechanarsBrand)
            , (Objectives.heirloomOfIarchon, Attachments.heirloomOfIarchon)
            , (Objectives.ordersFromAngmar, Attachments.ordersFromAngmar)
            ]
          controllerOf = \case
            AttachedTo (HeroTarget h) -> (.owner) <$> Map.lookup h heroes
            AttachedTo (AllyTarget a) -> placementController . (.placement) =<< Map.lookup a allies
            _ -> Nothing
          earned =
            [ (toCardCode book, seat)
            | o <- objectives
            , (clue, book) <- books
            , toCardCode o == toCardCode clue
            , Just pid <- [controllerOf o.placement]
            , Just seat <- [elemIndex pid order]
            ]
      TheTreacheryOfRhudaur <$> runMessage msg (attrs {scenarioCampaign = Just $ foldl' (\l' (code, seat) -> recordCount BooksEarned code seat l') l earned})
    _ -> TheTreacheryOfRhudaur <$> runMessage msg attrs
