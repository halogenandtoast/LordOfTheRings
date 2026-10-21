module LordOfTheRings.ShadowsOfMirkwood.AJourneyToRhosgobelSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (aJourneyToRhosgobel)
import TestImport

rhosgobelTest :: (PlayerId -> GameT ()) -> IO ()
rhosgobelTest = gameTestIn aJourneyToRhosgobel testDeck

wilyador :: GameT Target
wilyador = getsGame (toTarget . fromJustNote "no Wilyador" . find ((== toCardCode Objectives.wilyador) . toCardCode) . toList . (.entities.allies))

-- | Wilyador under the first player's control, as the setup leaves him.
putWilyador :: GameT Target
putWilyador = do
  run . AddToStaging =<< genEncounterCard Objectives.wilyador
  wilyador

spec :: Spec
spec = describe "A Journey to Rhosgobel" do
  it "sets out Rhosgobel, and Wilyador under the first player's control with 2 damage" . rhosgobelTest $ \pid -> do
    flipFirstQuest pid
    getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.rhosgobel]
    w <- wilyador
    getsGame (fmap (.placement) . find ((== w) . toTarget) . toList . (.entities.allies)) `shouldReturn` Just (InPlayArea pid)
    getCharacterDamage w `shouldReturn` 2

  it "deals Wilyador 2 damage at the end of each round" . rhosgobelTest $ \_ -> do
    w <- putWilyador
    run EndRound
    getCharacterDamage w `shouldReturn` 2

  it "heals Wilyador no more than 5 at once, and not at all while Rhosgobel is in the staging area" . rhosgobelTest $ \_ -> do
    w <- putWilyador
    run $ PlaceTokens GameSource w Damage 10
    run $ HealDamage GameSource w 10
    getCharacterDamage w `shouldReturn` 5
    _ <- setEncounterDeck [Locations.rhosgobel]
    run RevealEncounterCard
    run $ HealDamage GameSource w 5
    getCharacterDamage w `shouldReturn` 5

  it "loses the game when Wilyador leaves play" . rhosgobelTest $ \_ -> do
    w <- putWilyador
    run $ Discard w
    getsGame (.outcome) `shouldReturn` Just Lost

  it "wins at Return to Rhosgobel when the Athelas heal Wilyador fully" . rhosgobelTest $ \pid -> do
    w <- putWilyador
    run $ PlaceTokens GameSource w Damage 5
    run . AddToStaging =<< genEncounterCard Objectives.athelas
    athelas <- getsGame (toId . fromJustNote "no Athelas" . headMay . toList . (.entities.objectives))
    eowyn <- getHeroWithCode "01007"
    run $ ClaimObjective athelas (toTarget eowyn)
    stage <- putQuestStage Quests.returnToRhosgobel
    run $ Flip stage
    resolveWhenRevealed pid
    getsGame (.outcome) `shouldReturn` Just Won

  it "lets only Eagles or ranged characters defend against Mirkwood Flock" . rhosgobelTest $ \pid -> do
    _ <- setEncounterDeck [Enemies.mirkwoodFlock]
    run RevealEncounterCard
    flock <- getsGame (fromJustNote "no flock" . headMay . map toId . toList . (.entities.enemies))
    run $ EnemyAttack pid flock
    eowyn <- getHeroWithCode "01007"
    offered <- maybe False (any \case TargetLabel t _ -> t == toTarget eowyn; _ -> False) . fmap questionChoices <$> getQuestion pid
    offered `shouldBe` False

  it "pays for Creature cards with Radagast's resources" . rhosgobelTest $ \pid -> do
    radagast <- putAllyIntoPlay pid Allies.radagast
    run $ PlaceTokens GameSource radagast Resource 2
    guardian <- putCardIntoHand pid Allies.wingedGuardian
    run $ BeginPhase PlanningPhase
    chooseTarget pid guardian
    chooseOptionMatching "pay 2 from Radagast" pid \case
      TargetLabel _ msgs -> any (\case RemoveTokens _ _ Resource 2 -> True; _ -> False) msgs
      _ -> False
    getsGame (any ((== toCardCode Allies.wingedGuardian) . toCardCode) . toList . (.entities.allies)) `shouldReturn` True

  it "puts a destroyed hero back into play with Landroval" . rhosgobelTest $ \pid -> do
    landroval <- putAllyIntoPlay pid Allies.landroval
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ DealDamage GameSource eowyn 4
    chooseAbilityOf pid landroval
    back <- toTarget <$> getHeroWithCode "01007"
    getCharacterDamage back `shouldReturn` 1
