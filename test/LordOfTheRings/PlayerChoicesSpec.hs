-- | Rules about who chooses, in what order, and what is left over: abilities any player
-- may trigger, effects aimed at a player, orders the player picks, and "another" character.
module LordOfTheRings.PlayerChoicesSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Criteria (Criterion (DistinctCharacters))
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Window (passesCriteria)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (AllyMatcher (..), HeroMatcher (..), PlayerMatcher (..))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (aJourneyToRhosgobel, conflictAtTheCarrock, theDeadMarshes)
import LordOfTheRings.Trait (Trait (Rohan, Warrior))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseEnds), mkWhen)
import TestImport

-- | Two seats: the test deck (Beravor, Éowyn, Théodred) first, then the Dwarves of Durin
-- (with Bifur, a Lore hero).
table :: Scenario -> (PlayerId -> PlayerId -> GameT ()) -> IO ()
table scenario body = do
  seats <- traverse (fmap (either (error . show) id) . uncurry loadPlayer) (("one", testDeck) :| [("two", coreDeck "dwarves-of-durin")])
  env <- newGameEnv (newTableIn scenario seats) (const $ pure ()) 1
  runGameT env do
    order <- getsGame (.playerOrder)
    case order of
      [one, two] -> body one two
      _ -> error "two seats"

hero :: CardCode -> GameT Hero
hero = getHeroWithCode

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> heroExhausted . toAttrs <$> getsGame (fromJustNote "hero" . lookup hid . mapToList . (.entities.heroes))
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> pure False

playEvent :: PlayerId -> CardDef -> GameT ()
playEvent pid def = do
  card <- genPlayerCard def (Just pid)
  run $ PlayEvent pid card []

onTopOfDeck :: PlayerId -> CardDef -> GameT Card
onTopOfDeck pid def = do
  card <- PlayerCard <$> genPlayerCard def (Just pid)
  g <- getGame
  putGame $ g & playersL . ix pid . deckL %~ (card :)
  pure card

spec :: Spec
spec = describe "Player choices" do
  it "lets another player trigger Bifur, paying from their own hero" . table passageThroughMirkwood $ \one _ -> do
    bifur <- hero "03002"
    beravor <- hero "01012"
    run $ PlaceTokens GameSource (toTarget beravor) Resource 1
    run $ ActionWindow one
    chooseAbilityOf one bifur
    getHeroResources beravor.id `shouldReturn` 0
    getHeroResources bifur.id `shouldReturn` 1

  it "lets any player spend Lore on Grimbeorn the Old" . table conflictAtTheCarrock $ \_ two -> do
    run . AddToStaging =<< genEncounterCard Objectives.grimbeornTheOld
    grimbeorn <- getsGame (toTarget . fromJustNote "no Grimbeorn" . headMay . toList . (.entities.objectives))
    bifur <- hero "03002"
    run $ PlaceTokens GameSource (toTarget bifur) Resource 1
    run $ ActionWindow two
    chooseAbilityOf two grimbeorn
    getsGame (sum . map (.resources) . toList . (.entities.objectives)) `shouldReturn` 1
    getHeroResources bifur.id `shouldReturn` 0

  it "lowers the next Silvan ally any player plays after O Lórien!" . table passageThroughMirkwood $ \one two -> do
    eowyn <- hero "01007"
    lorien <- putAttachmentOn one Attachments.oLorien (toTarget eowyn)
    run $ ActionWindow one
    chooseAbilityOf one lorien
    card <- putCardIntoHand two Allies.naithGuide
    costModifier two card `shouldReturn` (-1)

  it "stops a Feinted enemy attacking that player only" . table passageThroughMirkwood $ \one two -> do
    run . AddToStaging =<< genEncounterCard Enemies.kingSpider
    eid <- getsGame (toId . fromJustNote "no enemy" . headMay . toList . (.entities.enemies))
    run $ EngageEnemy one eid
    playEvent one Events.feint
    chooseTarget one eid
    mods <- getModifiers eid
    (CannotAttackPlayer one `elem` mods, CannotAttack `elem` mods) `shouldBe` (True, False)
    run $ PlaceEnemy eid (EngagedWith two)
    run $ ChooseEnemyAttack two [eid]
    isJust <$> getQuestion two `shouldReturn` True

  describe "Galadriel" do
    it "spares an ally that entered play before she did, until she leaves" . gameTest $ \pid -> do
      guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      run . PutHeroIntoPlay pid =<< genPlayerCard Heroes.galadriel (Just pid)
      run $ CommitToQuest guard'
      isExhausted guard' `shouldReturn` False
      run $ Ready guard'
      run . Discard . toTarget =<< hero "08112"
      run $ CommitToQuest guard'
      isExhausted guard' `shouldReturn` True

  it "spares a Rohan character entering play after We Do Not Sleep" . gameTest $ \pid -> do
    playEvent pid Events.weDoNotSleep
    rider <- putAllyIntoPlay pid Allies.westfoldHorseBreaker
    run $ CommitToQuest rider
    isExhausted rider `shouldReturn` False

  it "returns Gildor Inglorion's cards in the order chosen" . gameTest $ \pid -> do
    [c3, c2, c1] <- traverse (onTopOfDeck pid) [Allies.guardOfTheCitadel, Allies.gondorianSpearman, Allies.wanderingTook]
    gildor <- putAllyIntoPlay pid Allies.gildorInglorion
    run $ ActionWindow pid
    chooseAbilityOf pid gildor
    chooseLabel "$label.done" pid
    -- each goes on top as chosen: c1, then c3, then c2 leaves c2 c3 c1
    traverse_ (chooseTarget pid) [c1, c3, c2]
    map (.id) . take 3 . (.deck) <$> getPlayer pid `shouldReturn` map (.id) [c2, c3, c1]

  it "needs a different character for each trait" . gameTest $ \pid -> do
    let pair t = (HeroControlledBy (PlayerWithId pid) <> HeroWithTrait t, AllyControlledBy (PlayerWithId pid) <> UniqueAlly <> AllyWithTrait t)
        both = DistinctCharacters [pair Rohan, pair Warrior]
    -- Éowyn is Rohan and Théodred a Rohan Warrior
    passesCriteria both `shouldReturn` True
    run . Discard . toTarget =<< hero "01007"
    passesCriteria both `shouldReturn` False

  it "puts The Red Arrow in the victory display" . gameTest $ \pid -> do
    theodred <- hero "01002"
    arrow <- putAttachmentOn pid Attachments.theRedArrow (toTarget theodred)
    run $ AddToVictoryDisplay arrow
    getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` [toCardCode Attachments.theRedArrow]
    getsGame (length . (.entities.attachments)) `shouldReturn` 0

  describe "The Dead Marshes" do
    let putGollum = run . AddToStaging =<< genEncounterCard Objectives.gollumTheDeadMarshes
        gollumResources = getsGame (sum . map (.resources) . toList . (.entities.objectives))
    it "asks each player in turn whether to exhaust a Lore hero for A Wisp of Pale Sheen" . table theDeadMarshes $ \one two -> do
      putGollum
      _ <- setEncounterDeck [Treacheries.aWispOfPaleSheen]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.aWispOfPaleSheen.allow" one
      bifur <- hero "03002"
      chooseTarget two bifur
      gollumResources `shouldReturn` 1
      isExhausted (toTarget bifur) `shouldReturn` True

    it "has every player commit to Gollum's escape test" . table theDeadMarshes $ \one two -> do
      putGollum
      _ <- setEncounterDeck [Locations.fensAndMires, Locations.fensAndMires, Locations.fensAndMires]
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      chooseTarget one =<< hero "01007"
      chooseLabel "$label.doneCommittingToEscape" one
      chooseTarget two =<< hero "03002"
      chooseLabel "$label.doneCommittingToEscape" two
      length <$> getEncounterDeck `shouldReturn` 1

  it "removes Radagast from the game after he heals Wilyador" . gameTestIn aJourneyToRhosgobel testDeck $ \pid -> do
    _ <- putQuestStage Quests.radagastsRequest
    run . AddToStaging =<< genEncounterCard Objectives.wilyador
    w <- getsGame (toTarget . fromJustNote "no Wilyador" . find ((== toCardCode Objectives.wilyador) . toCardCode) . toList . (.entities.allies))
    run $ PlaceTokens GameSource w Damage 3
    radagast <- putAllyIntoPlay pid Allies.radagast
    run $ PlaceTokens GameSource radagast Resource 1
    run $ ActionWindow pid
    chooseAbilityOf pid radagast
    chooseTarget pid w
    chooseOptionMatching "heal 1" pid \case
      Label {} -> True
      _ -> False
    getDamage w `shouldReturn` 2
    getsGame (any ((== toCardCode Allies.radagast) . toCardCode) . toList . (.entities.allies)) `shouldReturn` False
    any ((== toCardCode Allies.radagast) . toCardCode) . (.discard) <$> getPlayer pid `shouldReturn` False

  it "keeps player card effects off The Carrock" . gameTestIn conflictAtTheCarrock testDeck $ \pid -> do
    run . AddToStaging =<< genEncounterCard Locations.theCarrock
    carrock <- getsGame (toTarget . fromJustNote "no Carrock" . headMay . map toId . toList . (.entities.locations))
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    let fromAlly = case ally of
          AllyTarget aid -> AllySource aid
          _ -> error "not an ally"
    run $ PlaceTokens fromAlly carrock Progress 1
    run $ CreateEffect $ Effect fromAlly carrock [ThreatModifier (-1)] UntilEndOfPhase
    run $ PlaceTokens GameSource carrock Progress 1
    getsGame (fmap (.progress) . headMay . toList . (.entities.locations)) `shouldReturn` Just 1
    getModifiers carrock >>= (`shouldSatisfy` notElem (ThreatModifier (-1)))

  it "does not offer The Carrock as a player card's target" . gameTestIn conflictAtTheCarrock testDeck $ \pid -> do
    run . AddToStaging =<< genEncounterCard Locations.theCarrock
    run . AddToStaging =<< genEncounterCard Locations.fensAndMires
    locations <- getsGame (map (\l -> (toCardCode l, toTarget l)) . toList . (.entities.locations))
    bombur <- putAllyIntoPlay pid Allies.bombur
    run $ ActionWindow pid
    chooseAbilityOf pid bombur
    Just question <- getQuestion pid
    [code | TargetLabel t _ <- questionChoices question, (code, t') <- locations, t == t'] `shouldBe` [toCardCode Locations.fensAndMires]
