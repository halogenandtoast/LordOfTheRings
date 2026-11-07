module LordOfTheRings.Standalone.DolGuldurPowerChoicesSpec (spec) where

import LordOfTheRings.Matcher (getObjective)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur (attackOnDolGuldur)
import TestImport

stagePower :: Int -> GameT ObjectiveId
stagePower progress = do
  card <- genEncounterCard Objectives.powerOfLorien
  run $ AddToStaging card
  powers <- getsGame $ toList . (.entities.objectives)
  let oid = toId $ fromJustNote "Power missing" $ find ((== card.id) . (.cardId)) powers
  run $ PlaceTokens GameSource (ObjectiveTarget oid) Progress progress
  pure oid

crossRiver :: GameT ()
crossRiver = do
  target <- putQuestStage Quests.crossingTheAnduinDolGuldur
  run $ Flip target
  resolveRevealed

spec :: Spec
spec = describe "Dol Guldur multiple physical Powers of Lorien" do
  it "moves all stage progress to only the first player's chosen Power" . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
    untouched <- stagePower 4
    chosen <- stagePower 7
    stage <- putQuestStage Quests.assaultOnTheGoldenWood
    run $ Flip stage
    run $ PlaceTokens GameSource stage Progress 11
    active <- getActiveQuest
    run $ UseCardAbility pid (QuestSource $ toId active) 1 []
    chooseTarget pid chosen
    (.progress) <$> getActiveQuest `shouldReturn` 0
    (.progress) <$> getObjective untouched `shouldReturn` 4
    (.progress) <$> getObjective chosen `shouldReturn` 18

  it "pays both costs from the selected copy and leaves the other copy untouched" . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
    untouched <- stagePower 40
    chosen <- stagePower 30
    run $ ClaimObjective untouched $ PlayerTarget pid
    run . SetAside =<< genEncounterCard Objectives.hostOfLorien
    crossRiver
    chooseTarget pid chosen
    chooseLabel "$label.dolGuldur.takeHost" pid
    chooseLabel "$label.dolGuldur.takePower" pid
    (.progress) <$> getObjective untouched `shouldReturn` 40
    retained <- getObjective chosen
    retained.progress `shouldBe` 0
    retained.placement `shouldBe` AttachedTo (PlayerTarget pid)
    getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` [toCardCode Objectives.hostOfLorien]

  it "does not pool additional progress from a different copy" . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
    untouched <- stagePower 20
    chosen <- stagePower 10
    run . SetAside =<< genEncounterCard Objectives.hostOfLorien
    crossRiver
    chooseTarget pid chosen
    chooseLabel "$label.dolGuldur.takeHost" pid
    (.progress) <$> getObjective untouched `shouldReturn` 20
    getsGame (map toId . toList . (.entities.objectives)) `shouldReturn` [untouched]
    getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` [toCardCode Objectives.hostOfLorien]

  it "discards only the selected copy when the Host is declined" . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
    untouched <- stagePower 30
    chosen <- stagePower 30
    run . SetAside =<< genEncounterCard Objectives.hostOfLorien
    crossRiver
    chooseTarget pid chosen
    chooseLabel "$label.skip" pid
    (.progress) <$> getObjective untouched `shouldReturn` 30
    getsGame (map toId . toList . (.entities.objectives)) `shouldReturn` [untouched]
