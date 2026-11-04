module LordOfTheRings.VengeanceOfMordor.SmeagolSpec (spec) where

import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (EnemyAttrs (..))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (Traveled), mkAfter)
import TestImport

smeagolCode, gollumCode :: CardCode
smeagolCode = "21072"
gollumCode = "21073"

-- | Sméagol beside Beravor.
smeagolDeck :: DeckList
smeagolDeck = deckList [smeagolCode, "01012"] testDeck.cards

smeagolTest :: (PlayerId -> GameT ()) -> IO ()
smeagolTest = gameTestWith smeagolDeck

gollum :: GameT (Maybe EnemyAttrs)
gollum = getsGame (fmap toAttrs . find ((== gollumCode) . toCardCode) . toList . (.entities.enemies))

smeagol :: GameT (Maybe HeroAttrs)
smeagol = getsGame (fmap toAttrs . find ((== smeagolCode) . toCardCode) . toList . (.entities.heroes))

-- | Stinker revealed, an Old Forest Road under it for its surge.
revealStinker :: GameT ()
revealStinker = do
  _ <- setEncounterDeck [Treacheries.stinker, Locations.oldForestRoad]
  run RevealEncounterCard
  resolveRevealed

flipToGollum :: GameT EnemyId
flipToGollum = do
  revealStinker
  maybe (error "no Gollum") (.enemyId) <$> gollum

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

-- | The enemy attacks the player, the shadow card dealt to it first, left undefended.
undefendedShadowAttack :: PlayerId -> EnemyId -> CardDef -> GameT ()
undefendedShadowAttack pid eid shadow = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid
  chooseLabel "$label.takeAttackUndefended" pid
  passActions pid

spec :: Spec
spec = describe "Sméagol and Gollum" do
  it "shuffles 2 Stinkers into the encounter deck at setup" . smeagolTest $ \_ -> do
    _ <- setEncounterDeck []
    run $ DoStep 7 Setup
    stinkers <- length . filter ((== toCardCode Treacheries.stinker) . toCardCode) <$> getEncounterDeck
    stinkers `shouldBe` 2

  it "raises his threat by 1 to draw a card after the players travel" . smeagolTest $ \pid -> do
    lid <- do
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
    start <- threatOf pid
    hand <- length . (.hand) <$> getPlayer pid
    run $ CheckWindows [mkAfter $ Traveled lid]
    chooseAbilityOf pid . HeroTarget . (.heroId) . fromJustNote "no Sméagol" =<< smeagol
    threatOf pid `shouldReturn` (start + 1)
    (length . (.hand) <$> getPlayer pid) `shouldReturn` (hand + 1)

  it "cannot have attachments" . smeagolTest $ \_ -> do
    hid <- (.heroId) . fromJustNote "no Sméagol" <$> smeagol
    run $ CheckWindows []
    getModifiers (HeroTarget hid) >>= (`shouldSatisfy` elem CannotHaveAttachments)

  describe "Stinker" do
    it "flips Sméagol to Gollum, his tokens discarded, engaged with his owner" . smeagolTest $ \pid -> do
      hid <- (.heroId) . fromJustNote "no Sméagol" <$> smeagol
      run $ PlaceTokens GameSource (HeroTarget hid) Resource 2
      revealStinker
      smeagol `shouldReturn` Nothing
      g <- fromJustNote "no Gollum" <$> gollum
      g.enemyPlacement `shouldBe` EngagedWith pid
      g.enemyOwner `shouldBe` Just pid
      g.enemyTokens `shouldBe` mempty

    it "has Gollum make an immediate attack instead, while he is in play" . smeagolTest $ \pid -> do
      eid <- flipToGollum
      _ <- setEncounterDeck [Treacheries.stinker, Locations.oldForestRoad]
      run RevealEncounterCard
      resolveRevealed
      passActions pid
      getQuestion pid >>= \case
        Just (QuestionLabel l _ _ _) -> l `shouldSatisfy` ("$label.declareDefender" `isPrefixOf`)
        q -> liftIO $ expectationFailure ("no attack: " <> show (isJust q))
      (fmap (.enemyId) <$> gollum) `shouldReturn` Just eid

    it "raises your threat by 4 as Gollum's shadow, and goes back into the encounter deck" . smeagolTest $ \pid -> do
      eid <- flipToGollum
      start <- threatOf pid
      undefendedShadowAttack pid eid Treacheries.stinker
      threatOf pid `shouldReturn` (start + 4)
      (map toCardCode <$> getEncounterDeck) >>= (`shouldSatisfy` elem (toCardCode Treacheries.stinker))

    it "raises your threat by 2 as another enemy's shadow" . smeagolTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Enemies.kingSpider
      eid <- getsGame (fromJustNote "no spider" . headMay . map toId . toList . (.entities.enemies))
      run $ EngageEnemy pid eid
      start <- threatOf pid
      undefendedShadowAttack pid eid Treacheries.stinker
      threatOf pid `shouldReturn` (start + 2)

  describe "Gollum" do
    it "flips back to Sméagol, exhausted under his owner's control, when defeated" . smeagolTest $ \pid -> do
      eid <- flipToGollum
      run $ Destroy (PlayerAttackSource pid) (EnemyTarget eid)
      gollum `shouldReturn` Nothing
      h <- fromJustNote "no Sméagol" <$> smeagol
      h.heroOwner `shouldBe` pid
      h.heroExhausted `shouldBe` True

    it "is immune to non-Gollum encounter card effects" . smeagolTest $ \_ -> do
      eid <- flipToGollum
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
      run $ CreateEffect $ Effect (LocationSource lid) (EnemyTarget eid) [AttackModifier 3] UntilEndOfRound
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [DefenseModifier 1] UntilEndOfRound
      mods <- getModifiers (EnemyTarget eid)
      mods `shouldSatisfy` notElem (AttackModifier 3)
      mods `shouldSatisfy` elem (DefenseModifier 1)
      mods `shouldSatisfy` elem ImmuneToPlayerCardEffects

    it "does not eliminate a player whose only hero he was" . gameTestWith (deckList [smeagolCode] testDeck.cards) $ \pid -> do
      _ <- flipToGollum
      run CheckEliminations
      ((.eliminated) <$> getPlayer pid) `shouldReturn` False

    it "engages the first player, and follows the first player token" . twoPlayerTest smeagolDeck $ \one two -> do
      _ <- flipToGollum
      (fmap (.enemyPlacement) <$> gollum) `shouldReturn` Just (EngagedWith one)
      ((>>= (.enemyOwner)) <$> gollum) `shouldReturn` Just two
      run $ SetFirstPlayer two
      (fmap (.enemyPlacement) <$> gollum) `shouldReturn` Just (EngagedWith two)

    it "leaves play with his eliminated owner" . twoPlayerTest smeagolDeck $ \_ two -> do
      _ <- flipToGollum
      run $ EliminatePlayer two
      gollum `shouldReturn` Nothing
      (map toCardCode . (.discard) <$> getPlayer two) >>= (`shouldSatisfy` elem gollumCode)
