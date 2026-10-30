module LordOfTheRings.AngmarAwakened.TheBattleOfCarnDumSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEngagementCost)
import LordOfTheRings.Keyword (Keyword (Battle))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (GainKeyword, WillpowerModifier))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theBattleOfCarnDum)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (When), Window (..), WindowType (RoundEnds))
import TestImport

carnDumTest :: (PlayerId -> GameT ()) -> IO ()
carnDumTest = gameTestIn theBattleOfCarnDum testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

active :: CardDef -> GameT LocationId
active def = do
  run . PutIntoPlayAsActiveLocation =<< genEncounterCard def
  getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

-- | The side Thaurdir shows, by his card code.
thaurdirSide :: GameT (Maybe CardCode)
thaurdirSide =
  getsGame (fmap toCardCode . find ((`elem` [toCardCode Enemies.thaurdirCaptain, toCardCode Enemies.thaurdirChampion]) . toCardCode) . toList . (.entities.enemies))

shadowsOn :: EnemyId -> GameT Int
shadowsOn eid = length . (.shadowCards) <$> getEnemy eid

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | Take the attack being made undefended, its shadow cards turned over in order and the
-- damage going to the first hero offered.
undefended :: PlayerId -> GameT ()
undefended pid = go (10 :: Int)
 where
  go 0 = pure ()
  go n = do
    passActions pid
    resolveShadowsInOrder pid
    getQuestion pid >>= \case
      Just q
        | any (\case Label l _ -> l == "$label.takeAttackUndefended"; _ -> False) (questionChoices q) ->
            chooseLabel "$label.takeAttackUndefended" pid >> go (n - 1)
        | any (\case TargetLabel (HeroTarget _) _ -> True; _ -> False) (questionChoices q) ->
            chooseOptionMatching "damage a hero" pid (\case TargetLabel (HeroTarget _) _ -> True; _ -> False) >> go (n - 1)
        | any (\case Label l _ -> l == "$label.pass"; _ -> False) (questionChoices q) ->
            chooseLabel "$label.pass" pid >> go (n - 1)
      _ -> pure ()

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

spec :: Spec
spec = describe "The Battle of Carn Dûm" do
  it "is a scenario the game can be dealt" . carnDumTest $ \_ ->
    isJust (lookupScenario "the-battle-of-carn-dum") `shouldBe` True

  it "adds Thaurdir and a Carn Dûm Garrison per player to the staging area, and makes Accursed Battlefield active" . carnDumTest $ \pid -> do
    flipFirstQuest pid
    thaurdirSide `shouldReturn` Just (toCardCode Enemies.thaurdirCaptain)
    placementOf Enemies.thaurdirCaptain `shouldReturn` Just StagingArea
    placementOf Enemies.carnDumGarrison `shouldReturn` Just StagingArea
    placementOf Locations.accursedBattlefield `shouldReturn` Just ActiveLocation
    g <- getGame
    length [e | e <- toList g.entities.enemies, toCardCode e == toCardCode Enemies.carnDumGarrison] `shouldBe` 1
    deck <- map toCardCode <$> getEncounterDeck
    deck `shouldSatisfy` notElem (toCardCode Enemies.thaurdirCaptain)
    deck `shouldSatisfy` notElem (toCardCode Enemies.thaurdirChampion)
    deck `shouldSatisfy` elem (toCardCode Quests.furiousCharge)

  -- \* Thaurdir

  it "gives each other enemy +10 engagement cost as Captain, -10 as Champion" . carnDumTest $ \_ -> do
    thaurdir <- staged Enemies.thaurdirCaptain
    garrison <- staged Enemies.carnDumGarrison
    getEngagementCost garrison `shouldReturn` 50
    getEngagementCost thaurdir `shouldReturn` 50
    _ <- setEncounterDeck [Enemies.werewolfOfAngmar]
    run $ Flip (toTarget thaurdir)
    getEngagementCost garrison `shouldReturn` 30

  it "heals 3 damage and attacks the first player as he flips to his Champion side" . carnDumTest $ \pid -> do
    thaurdir <- staged Enemies.thaurdirCaptain
    run $ PlaceTokens GameSource (toTarget thaurdir) Damage 4
    _ <- setEncounterDeck [Enemies.werewolfOfAngmar]
    run $ Flip (toTarget thaurdir)
    thaurdirSide `shouldReturn` Just (toCardCode Enemies.thaurdirChampion)
    getDamage (toTarget thaurdir) `shouldReturn` 1
    undefended pid
    -- the 6 damage destroys Beravor, or lands on a hero that survives it
    heroes <- getsGame (toList . (.entities.heroes))
    (length heroes < 3 || any ((> 0) . (.damage)) heroes) `shouldBe` True

  it "deals each enemy a shadow card as he flips to his Captain side, or a Sorcery treachery is revealed" . carnDumTest $ \pid -> do
    thaurdir <- staged Enemies.thaurdirCaptain
    garrison <- staged Enemies.carnDumGarrison
    _ <- setEncounterDeck [Enemies.werewolfOfAngmar]
    run $ Flip (toTarget thaurdir)
    undefended pid
    _ <- setEncounterDeck [Enemies.werewolfOfAngmar, Enemies.werewolfOfAngmar]
    run $ Flip (toTarget thaurdir)
    thaurdirSide `shouldReturn` Just (toCardCode Enemies.thaurdirCaptain)
    shadowsOn garrison `shouldReturn` 1
    reveal [Treacheries.theSkyDarkens, Enemies.werewolfOfAngmar, Enemies.werewolfOfAngmar]
    shadowsOn garrison `shouldReturn` 2

  -- \* The Clutches of Carn Dûm

  it "flips Thaurdir once he has 3 shadow cards, and back at the end of the round" . carnDumTest $ \pid -> do
    flipFirstQuest pid
    thaurdir <- enemyOf Enemies.thaurdirCaptain
    _ <- setEncounterDeck (replicate 4 Enemies.werewolfOfAngmar)
    run $ DealShadowCard thaurdir
    run $ DealShadowCard thaurdir
    thaurdirSide `shouldReturn` Just (toCardCode Enemies.thaurdirCaptain)
    run $ DealShadowCard thaurdir
    thaurdirSide `shouldReturn` Just (toCardCode Enemies.thaurdirChampion)
    undefended pid
    _ <- setEncounterDeck (replicate 3 Enemies.werewolfOfAngmar)
    run $ CheckWindows [Window When RoundEnds]
    thaurdirSide `shouldReturn` Just (toCardCode Enemies.thaurdirCaptain)

  it "keeps Thaurdir from taking damage, and keeps unresolved shadow cards at the end of the combat phase" . carnDumTest $ \pid -> do
    flipFirstQuest pid
    thaurdir <- enemyOf Enemies.thaurdirCaptain
    garrison <- enemyOf Enemies.carnDumGarrison
    run $ DealDamage GameSource (toTarget thaurdir) 3
    getDamage (toTarget thaurdir) `shouldReturn` 0
    _ <- setEncounterDeck [Enemies.werewolfOfAngmar]
    run $ DealShadowCard garrison
    run $ EndPhase CombatPhase
    shadowsOn garrison `shouldReturn` 1

  it "discards shadow cards at the end of the combat phase in another stage" . carnDumTest $ \_ -> do
    _ <- putQuestStage Quests.midwintersCrux
    garrison <- staged Enemies.carnDumGarrison
    _ <- setEncounterDeck [Enemies.werewolfOfAngmar]
    run $ DealShadowCard garrison
    run $ EndPhase CombatPhase
    shadowsOn garrison `shouldReturn` 0

  -- \* Midwinter's Crux

  it "raises each player's threat by the shadow cards in play and flips a Captain Thaurdir as it is revealed" . carnDumTest $ \pid -> do
    thaurdir <- staged Enemies.thaurdirCaptain
    _ <- setEncounterDeck (replicate 3 Enemies.werewolfOfAngmar)
    run $ DealShadowCard thaurdir
    run $ DealShadowCard thaurdir
    threat <- threatOf pid
    q <- putQuestStage Quests.midwintersCrux
    run $ Flip q
    resolveWhenRevealed pid
    threatOf pid `shouldReturn` threat + 2
    thaurdirSide `shouldReturn` Just (toCardCode Enemies.thaurdirChampion)

  it "makes Thaurdir lose indestructible at 15 progress, and his defeat wins the game" . carnDumTest $ \pid -> do
    q <- putQuestStage Quests.midwintersCrux
    run $ Flip q
    resolveWhenRevealed pid
    thaurdir <- staged Enemies.thaurdirCaptain
    run $ DealDamage GameSource (toTarget thaurdir) 10
    placementOf Enemies.thaurdirCaptain `shouldReturn` Just StagingArea
    run $ PlaceTokens GameSource q Progress 20
    outcome `shouldReturn` Just Won

  -- \* Enemies

  it "reduces the progress placed on the current quest by 1 for each Orc Grunts" . carnDumTest $ \_ -> do
    q <- putQuestStage Quests.theClutchesOfCarnDum
    _ <- staged Enemies.orcGrunts
    run $ PlaceTokens GameSource q Progress 3
    (.progress) <$> getActiveQuest `shouldReturn` 2

  it "gives Carn Dûm Garrison +1 attack for each shadow card dealt to it" . carnDumTest $ \_ -> do
    garrison <- staged Enemies.carnDumGarrison
    _ <- setEncounterDeck [Enemies.werewolfOfAngmar, Enemies.werewolfOfAngmar]
    run $ DealShadowCard garrison
    run $ DealShadowCard garrison
    getEnemyAttack garrison `shouldReturn` 6

  it "engages and attacks the first player with Werewolf of Angmar once it has 3 shadow cards in the staging area" . carnDumTest $ \pid -> do
    werewolf <- staged Enemies.werewolfOfAngmar
    _ <- setEncounterDeck (replicate 3 Locations.accursedBattlefield)
    run $ DealShadowCard werewolf
    run $ DealShadowCard werewolf
    placementOf Enemies.werewolfOfAngmar `shouldReturn` Just StagingArea
    run $ DealShadowCard werewolf
    placementOf Enemies.werewolfOfAngmar `shouldReturn` Just (EngagedWith pid)
    getQuestion pid >>= (`shouldSatisfy` isJust)

  -- \* Locations

  it "gives each quest card battle while Accursed Battlefield is active" . carnDumTest $ \_ -> do
    q <- putQuestStage Quests.theClutchesOfCarnDum
    _ <- active Locations.accursedBattlefield
    getModifiers q >>= (`shouldSatisfy` elem (GainKeyword Battle))

  it "gives a shadow card with no shadow effect one that raises threat by 2 while Mountains of Angmar is active" . carnDumTest $ \pid -> do
    _ <- active Locations.mountainsOfAngmar
    garrison <- staged Enemies.carnDumGarrison
    run $ EngageEnemy pid garrison
    _ <- setEncounterDeck [Enemies.werewolfOfAngmar]
    run $ DealShadowCard garrison
    threat <- threatOf pid
    run $ EnemyAttack pid garrison
    undefended pid
    threatOf pid `shouldReturn` threat + 2

  it "deals each enemy a shadow card to travel to Blight of Carn Dûm" . carnDumTest $ \_ -> do
    garrison <- staged Enemies.carnDumGarrison
    run . AddToStaging =<< genEncounterCard Locations.blightOfCarnDum
    blight <- getsGame (fromJustNote "no blight" . fmap toId . headMay . toList . (.entities.locations))
    _ <- setEncounterDeck [Enemies.werewolfOfAngmar]
    run $ TravelTo blight
    shadowsOn garrison `shouldReturn` 1

  it "raises a player's threat for each resource spent while Fortress Walls is active" . carnDumTest $ \pid -> do
    _ <- active Locations.fortressWalls
    beravor <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource beravor Resource 2
    threat <- threatOf pid
    run $ RemoveTokens (PlayerSource pid) beravor Resource 2
    threatOf pid `shouldReturn` threat + 2

  -- \* Treacheries

  it "discards a random card and each other copy of it from the deck with The Sky Darkens" . carnDumTest $ \pid -> do
    g <- getGame
    card <- PlayerCard <$> genPlayerCard Allies.guardOfTheCitadel (Just pid)
    copy <- PlayerCard <$> genPlayerCard Allies.guardOfTheCitadel (Just pid)
    putGame $ g & playersL . ix pid . handL .~ [card] & playersL . ix pid . deckL %~ (copy :)
    copies <- length . filter ((== toCardCode Allies.guardOfTheCitadel) . toCardCode) . (.deck) <$> getPlayer pid
    reveal [Treacheries.theSkyDarkens]
    map toCardCode . (.discard) <$> getPlayer pid `shouldReturn` replicate (copies + 1) (toCardCode Allies.guardOfTheCitadel)
    length . filter ((== toCardCode Allies.guardOfTheCitadel) . toCardCode) . (.deck) <$> getPlayer pid `shouldReturn` 0

  it "has each player distribute damage among questing characters for the costliest card in his discard pile with Vile Affliction" . carnDumTest $ \pid -> do
    g <- getGame
    card <- PlayerCard <$> genPlayerCard Allies.guardOfTheCitadel (Just pid)
    putGame $ g & playersL . ix pid . discardL .~ [card]
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ CommitToQuest eowyn
    reveal [Treacheries.vileAffliction]
    chooseTarget pid eowyn
    chooseTarget pid eowyn
    getDamage eowyn `shouldReturn` 2

  it "flips Thaurdir with Daechanar's Will, surging once he is Captain again" . carnDumTest $ \pid -> do
    thaurdir <- staged Enemies.thaurdirCaptain
    reveal [Treacheries.daechanarsWill, Enemies.werewolfOfAngmar]
    thaurdirSide `shouldReturn` Just (toCardCode Enemies.thaurdirChampion)
    undefended pid
    -- the Werewolf was dealt to the Champion's attack, not revealed
    placementOf Enemies.werewolfOfAngmar `shouldReturn` Nothing
    _ <- setEncounterDeck [Treacheries.daechanarsWill, Locations.accursedBattlefield, Locations.fortressWalls]
    run RevealEncounterCard
    resolveRevealed
    thaurdirSide `shouldReturn` Just (toCardCode Enemies.thaurdirCaptain)
    shadowsOn thaurdir `shouldReturn` 1
    placementOf Locations.fortressWalls `shouldReturn` Just StagingArea

  -- \* Furious Charge

  it "lowers each character's willpower and flips Thaurdir as progress is placed on Furious Charge, and adds 10 progress to the main quest once defeated" . carnDumTest $ \pid -> do
    _ <- putQuestStage Quests.theClutchesOfCarnDum
    _ <- staged Enemies.thaurdirCaptain
    reveal [Quests.furiousCharge]
    charge <- getsGame (fromJustNote "no side quest" . fmap toTarget . find ((== toCardCode Quests.furiousCharge) . toCardCode) . toList . (.entities.quests))
    _ <- setEncounterDeck [Enemies.werewolfOfAngmar]
    run $ PlaceTokens GameSource charge Progress 1
    thaurdirSide `shouldReturn` Just (toCardCode Enemies.thaurdirChampion)
    undefended pid
    -- the undefended attack may have destroyed a hero, so read one still in play
    mods <- getModifiers . toTarget =<< getsGame (fromJustNote "no hero" . headMay . toList . (.entities.heroes))
    mods `shouldSatisfy` elem (WillpowerModifier (-2))
    run $ PlaceTokens GameSource charge Progress 4
    passActions pid
    main <- getsGame (fromJustNote "no main quest" . find ((== toCardCode Quests.theClutchesOfCarnDum) . toCardCode) . toList . (.entities.quests))
    main.progress `shouldBe` 10
