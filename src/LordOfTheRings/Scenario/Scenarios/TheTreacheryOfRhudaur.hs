module LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur (theTreacheryOfRhudaur) where

import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (PoolCard (..), addToPool)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Entities (entitiesAllies, entitiesHeroes, entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo), placementController)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (angmarSetup, thaurdirsBurdens)

newtype TheTreacheryOfRhudaur = TheTreacheryOfRhudaur ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTreacheryOfRhudaur :: Scenario
theTreacheryOfRhudaur = Scenario $ scenario TheTreacheryOfRhudaur "the-treachery-of-rhudaur" "The Treachery of Rhudaur"

-- | The Treachery of Rhudaur, Cursed Dead and Ruins of Arnor encounter sets.
instance RunMessage TheTreacheryOfRhudaur where
  runMessage msg (TheTreacheryOfRhudaur attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM (gatherEncounterSet attrs) [EncounterSet.TheTreacheryOfRhudaur, EncounterSet.CursedDead, EncounterSet.RuinsOfArnor]
      firstStage <- genEncounterCard Quests.secretsOfRhudaur
      laterStages <- traverse genEncounterCard [Quests.thaurdirsPursuit]
      putQuestIntoPlay firstStage
      pure $ TheTreacheryOfRhudaur $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Campaign: what the pool brings, Thaurdir's burden on Thaurdir among it
    DoStep 9 Setup | isJust attrs.campaign -> do
      angmarSetup attrs
      thaurdirsBurdens attrs False
      TheTreacheryOfRhudaur <$> runMessage msg attrs
    CheckWindows _ | isJust attrs.campaign -> do
      thaurdirsBurdens attrs False
      TheTreacheryOfRhudaur <$> runMessage msg attrs
    -- Resolution: For each Clue a player controls, that player may add the book with the
    -- same name to the Campaign Pool and their deck.
    -- ponytail: each book is added, a boon there being no reason to refuse
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
            [ PoolCard (toCardCode book) (elemIndex pid order) Nothing
            | o <- objectives
            , (clue, book) <- books
            , toCardCode o == toCardCode clue
            , Just pid <- [controllerOf o.placement]
            ]
      TheTreacheryOfRhudaur <$> runMessage msg (attrs {scenarioCampaign = Just $ foldl' (flip addToPool) l earned})
    _ -> TheTreacheryOfRhudaur <$> runMessage msg attrs
