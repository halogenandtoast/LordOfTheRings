-- | The player cards of Flight of the Stormcaller.
module LordOfTheRings.DreamChaser.FlightOfTheStormcallerPlayerCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (cdCardTraits)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfAttack, UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher (getEnemy, getThreat)
import LordOfTheRings.Modifier (ModifierType (CannotDiscardFromHand, Defending, GainTrait))
import LordOfTheRings.Phase
import LordOfTheRings.Trait (Trait (Gondor))
import LordOfTheRings.Window (WindowType (EnemyDestroyedByAttack), mkAfter)
import TestImport

beravorCode, eowynCode, theodredCode, gimliCode, eleanorCode, denethorCode :: CardCode
beravorCode = "01012"
eowynCode = "01007"
theodredCode = "01002"
gimliCode = "01004"
eleanorCode = "01008"
denethorCode = "12001"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

resources :: CardCode -> GameT Int
resources code = (.resources) <$> getHeroWithCode code

giveResources :: CardCode -> Int -> GameT ()
giveResources code n = do
  h <- hero code
  run $ PlaceTokens GameSource h Resource n

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

setThreat :: PlayerId -> Int -> GameT ()
setThreat pid n = putGame . (playersL . ix pid . threatL .~ n) =<< getGame

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> maybe False (heroExhausted . toAttrs) . lookup hid . mapToList <$> getsGame (.entities.heroes)
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> pure False

staged :: CardDef -> GameT EnemyId
staged def = do
  was <- getsGame (map toId . toList . (.entities.enemies))
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no enemy" . find (`notElem` was) . map toId . toList . (.entities.enemies))

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- staged def
  run $ EngageEnemy pid eid
  pure eid

defending :: Target -> GameT ()
defending t = run $ CreateEffect $ Effect GameSource t [Defending] UntilEndOfAttack

withHeroes :: [CardCode] -> (PlayerId -> GameT ()) -> IO ()
withHeroes codes = gameTestWith testDeck {heroes = codes}

offersCard :: PlayerId -> Card -> GameT Bool
offersCard pid card =
  maybe False (any \case TargetLabel t _ -> t == CardIdTarget card.id; _ -> False) . fmap questionChoices
    <$> getQuestion pid

spec :: Spec
spec = describe "Flight of the Stormcaller player cards" do
  describe "Denethor (leadership)" do
    it "adds 2 resources to his pool at setup" . withHeroes [denethorCode, eleanorCode, beravorCode] $ \_ -> do
      run $ DoStep 3 Setup
      resources denethorCode `shouldReturn` 2

    it "moves 1 resource to another Gondor hero, once a round" . withHeroes [denethorCode, eleanorCode, beravorCode] $ \pid -> do
      giveResources denethorCode 2
      denethor <- hero denethorCode
      run $ ActionWindow pid
      chooseAbilityOf pid denethor
      resources denethorCode `shouldReturn` 1
      resources eleanorCode `shouldReturn` 1
      offersAbilityOf pid denethor `shouldReturn` False

  describe "Rod of the Steward" do
    it "spends 2 resources from the attached hero to draw a card" . withHeroes [denethorCode, eleanorCode, beravorCode] $ \pid -> do
      denethor <- hero denethorCode
      rod <- putAttachmentOn pid Attachments.rodOfTheSteward denethor
      giveResources denethorCode 2
      was <- handSize pid
      run $ ActionWindow pid
      chooseAbilityOf pid rod
      handSize pid `shouldReturn` was + 1
      resources denethorCode `shouldReturn` 0

  describe "In Service of the Steward" do
    it "gives the attached character the Gondor trait" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      _ <- putAttachmentOn pid Attachments.inServiceOfTheSteward beravor
      getModifiers beravor >>= (`shouldSatisfy` elem (GainTrait Gondor))

  describe "Azain Silverbeard" do
    it "spends 1 tactics resource to deal 2 damage to another enemy sharing a trait" . withHeroes [gimliCode, eowynCode, theodredCode] $ \pid -> do
      azain <- putAllyIntoPlay pid Allies.azainSilverbeard
      destroyed <- staged Enemies.forestSpider
      other <- staged Enemies.forestSpider
      giveResources gimliCode 1
      run $ CheckWindows [mkAfter $ EnemyDestroyedByAttack destroyed [azain] (cdCardTraits Enemies.forestSpider)]
      chooseAbilityOf pid azain
      getDamage (EnemyTarget other) `shouldReturn` 2
      resources gimliCode `shouldReturn` 0

    it "may be triggered by any player" . twoPlayerTest (deckList [gimliCode, eowynCode, theodredCode] testDeck.cards) $ \one two -> do
      azain <- putAllyIntoPlay one Allies.azainSilverbeard
      destroyed <- staged Enemies.forestSpider
      other <- staged Enemies.forestSpider
      gimli <- heroOf two gimliCode
      run $ PlaceTokens GameSource gimli Resource 1
      run $ CheckWindows [mkAfter $ EnemyDestroyedByAttack destroyed [azain] (cdCardTraits Enemies.forestSpider)]
      chooseAbilityOf two azain
      getDamage (EnemyTarget other) `shouldReturn` 2

  describe "Last Stand" do
    it "deals the attacker damage equal to the printed attack of a Warrior destroyed defending" . withHeroes [gimliCode, eowynCode, theodredCode] $ \pid -> do
      eid <- engaged pid Enemies.forestSpider
      guardian <- putAllyIntoPlay pid Allies.guardianOfRivendell
      stand <- putCardIntoHand pid Events.lastStand
      defending guardian
      run $ Destroy (EnemyAttackSource eid) guardian
      chooseTarget pid (CardIdTarget stand.id)
      getDamage (EnemyTarget eid) `shouldReturn` 2

    it "is not offered for a Warrior not defending" . withHeroes [gimliCode, eowynCode, theodredCode] $ \pid -> do
      eid <- engaged pid Enemies.forestSpider
      guardian <- putAllyIntoPlay pid Allies.guardianOfRivendell
      stand <- putCardIntoHand pid Events.lastStand
      run $ Destroy (EnemyAttackSource eid) guardian
      offersCard pid stand `shouldReturn` False

  describe "Tides of Fate" do
    it "gives the defender +3 defense when a shadow card raises the attack" . gameTest $ \pid -> do
      eid <- engaged pid Enemies.forestSpider
      shadow <- genEncounterCard Enemies.eastBightPatrol
      run $ AttachShadowCard eid shadow
      eowyn <- hero eowynCode
      defending eowyn
      tides <- putCardIntoHand pid Events.tidesOfFate
      run $ ResolveShadowCard shadow.id eid pid [eowyn]
      chooseTarget pid (CardIdTarget tides.id)
      getDefense eowyn `shouldReturn` 4

    it "lets the group spend 2 tactics to ready the defender with +3 attack" . withHeroes [gimliCode, eowynCode, theodredCode] $ \pid -> do
      eid <- engaged pid Enemies.forestSpider
      shadow <- genEncounterCard Enemies.eastBightPatrol
      run $ AttachShadowCard eid shadow
      gimli <- hero gimliCode
      giveResources gimliCode 2
      run $ Exhaust gimli
      defending gimli
      tides <- putCardIntoHand pid Events.tidesOfFate
      run $ ResolveShadowCard shadow.id eid pid [gimli]
      chooseTarget pid (CardIdTarget tides.id)
      chooseLabel "$label.tidesOfFate.ready" pid
      replicateM_ 2 $ chooseTarget pid gimli
      isExhausted gimli `shouldReturn` False
      getAttack gimli `shouldReturn` 5

  describe "Imladris Caregiver" do
    it "discards a card to heal 1 damage, twice a round" . gameTest $ \pid -> do
      beravor <- hero beravorCode
      run $ DealDamage GameSource beravor 3
      caregiver <- putAllyIntoPlay pid Allies.imladrisCaregiver
      replicateM_ 2 $ putCardIntoHand pid Allies.gondorianSpearman
      let heal = do
            run $ ActionWindow pid
            chooseAbilityOf pid caregiver
            Just card <- headMay . (.hand) <$> getPlayer pid
            chooseTarget pid (CardIdTarget card.id)
      heal
      getDamage beravor `shouldReturn` 2
      heal
      getDamage beravor `shouldReturn` 1
      run $ ActionWindow pid
      offersAbilityOf pid caregiver `shouldReturn` False

  describe "Guardian of Rivendell" do
    it "costs discarding 2 cards from hand to play" . gameTest $ \pid -> do
      was <- handSize pid
      replicateM_ 3 $ putCardIntoHand pid Allies.gondorianSpearman
      card <- playWith pid theodredCode 3 Allies.guardianOfRivendell
      replicateM_ 2 do
        Just c <- headMay . filter ((/= card.id) . (.id)) . (.hand) <$> getPlayer pid
        chooseTarget pid (CardIdTarget c.id)
      handSize pid `shouldReturn` was + 1
      map toCardCode . toList <$> getsGame (.entities.allies) `shouldReturn` [toCardCode Allies.guardianOfRivendell]

    it "cannot be played without 2 other cards in hand" . gameTest $ \pid -> do
      putGame . (playersL . ix pid . handL .~ []) =<< getGame
      _ <- putCardIntoHand pid Allies.gondorianSpearman
      card <- putCardIntoHand pid Allies.guardianOfRivendell
      giveResources theodredCode 3
      run $ BeginPhase PlanningPhase
      offersCard pid card `shouldReturn` False

    it "cannot be played by a player who cannot discard from hand" . gameTest $ \pid -> do
      replicateM_ 2 $ putCardIntoHand pid Allies.gondorianSpearman
      card <- putCardIntoHand pid Allies.guardianOfRivendell
      giveResources theodredCode 3
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDiscardFromHand] UntilEndOfRound
      run $ BeginPhase PlanningPhase
      offersCard pid card `shouldReturn` False

  describe "Vanish from Sight" do
    it "treats your threat as 20 until the end of the phase" . withHeroes [beravorCode, eowynCode] $ \pid -> do
      setThreat pid 35
      vanish <- putCardIntoHand pid Events.vanishFromSight
      eid <- staged Enemies.forestSpider
      run $ ActionWindow pid
      chooseTarget pid (CardIdTarget vanish.id)
      getThreat pid `shouldReturn` 20
      run $ EngagementCheck pid 0
      (.placement) <$> getEnemy eid `shouldReturn` StagingArea

    it "cannot be played with 3 heroes" . gameTest $ \pid -> do
      vanish <- putCardIntoHand pid Events.vanishFromSight
      run $ ActionWindow pid
      offersCard pid vanish `shouldReturn` False
