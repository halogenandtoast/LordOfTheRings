module LordOfTheRings.DreamChaserEngineSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Public (publicGameFor)
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Scenario.Types (unchartedDeckL)
import TestImport

-- | The location in play whose card is the card given, whichever side is faceup.
locationOf :: EncounterCard -> GameT LocationAttrs
locationOf card = getsGame (fromJustNote "no such location" . find ((== card.id) . locationCardId) . map toAttrs . toList . (.entities.locations))

staged :: CardDef -> GameT (EncounterCard, LocationId)
staged def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  (card,) . locationId <$> locationOf card

unchartedDeck :: GameT [CardCode]
unchartedDeck = getsGame (map toCardCode . (.unchartedDeck) . toAttrs . (.scenario))

-- | Whether the location's facedown side is in the game as the player is sent it.
seesFacedownSide :: PlayerId -> LocationId -> GameT Bool
seesFacedownSide pid lid = do
  g <- getGame
  let locations =
        parseMaybe (withObject "game" \o -> o .: "entities" >>= (.: "locations")) (publicGameFor (== pid) g)
          :: Maybe (Map LocationId Value)
  pure case lookup lid =<< locations of
    Just (Object o) -> maybe False (/= Null) (KeyMap.lookup "otherSide" o)
    _ -> False

enemyWithCode :: CardDef -> GameT (Maybe EnemyId)
enemyWithCode def = getsGame (fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

allyWithCode :: CardDef -> GameT (Maybe AllyId)
allyWithCode def = getsGame (fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

spec :: Spec
spec = describe "The Dream-chaser engine" do
  describe "Uncharted locations" do
    it "enter play Lost Island side faceup, with no quest points" . gameTest $ \_ -> do
      (card, _) <- staged Locations.lushJungle
      l <- locationOf card
      locationCardCode l `shouldBe` toCardCode Locations.lostIsland
      l.otherSide `shouldBe` Just (toCardCode Locations.lushJungle)
      l.questPoints `shouldBe` 0
      l.threat `shouldBe` 2

    it "turn over to their other side, losing their progress" . gameTest $ \_ -> do
      (card, lid) <- staged Locations.lushJungle
      run $ PlaceTokens GameSource (toTarget lid) Progress 3
      run $ Flip (toTarget lid)
      l <- locationOf card
      locationCardCode l `shouldBe` toCardCode Locations.lushJungle
      l.otherSide `shouldBe` Just (toCardCode Locations.lostIsland)
      l.progress `shouldBe` 0
      l.questPoints `shouldBe` 4

    it "flip over as they become the active location" . gameTest $ \_ -> do
      (card, lid) <- staged Locations.cursedTemple
      run $ PlaceLocation lid ActiveLocation
      l <- locationOf card
      locationCardCode l `shouldBe` toCardCode Locations.cursedTemple
      l.placement `shouldBe` ActiveLocation

    it "show their facedown side only to the player who removed 4 progress from Lost Island to look" . gameTest $ \pid -> do
      (card, lid) <- staged Locations.cursedTemple
      seesFacedownSide pid lid `shouldReturn` False
      run $ PlaceTokens GameSource (toTarget lid) Progress 4
      run $ ActionWindow pid
      chooseAbilityOf pid lid
      l <- locationOf card
      l.lookedAt `shouldBe` [pid]
      l.progress `shouldBe` 0
      seesFacedownSide pid lid `shouldReturn` True
      g <- getGame
      other <- getRandom
      let hidden = parseMaybe (withObject "game" \o -> o .: "entities" >>= (.: "locations")) (publicGameFor (== other) g) :: Maybe (Map LocationId Value)
      (lookup lid =<< hidden) `shouldSatisfy` \case
        Just (Object o) -> not (KeyMap.member "otherSide" o)
        _ -> False

    it "return to the Uncharted deck, not the discard pile, once explored" . gameTest $ \_ -> do
      (_, lid) <- staged Locations.cursedTemple
      run $ PlaceLocation lid ActiveLocation
      run $ PlaceTokens GameSource (toTarget lid) Progress 3
      unchartedDeck `shouldReturn` [toCardCode Locations.cursedTemple]
      map toCardCode <$> getEncounterDiscard `shouldReturn` []
      getsGame (length . (.entities.locations)) `shouldReturn` 0

    it "are added to the staging area from the top of the Uncharted deck" . gameTest $ \_ -> do
      cards <- traverse genEncounterCard [Locations.floodedRuins, Locations.drownedGraves]
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (unchartedDeckL .~ cards)
      run $ AddFromUnchartedDeck 1
      l <- locationOf (fromJustNote "none" $ headMay cards)
      l.placement `shouldBe` StagingArea
      locationCardCode l `shouldBe` toCardCode Locations.lostIsland
      unchartedDeck `shouldReturn` [toCardCode Locations.drownedGraves]

    it "shuffle in the staging area: their progress removed, what was seen forgotten, nothing to follow them by" . gameTest $ \pid -> do
      (jungle, a) <- staged Locations.lushJungle
      (temple, b) <- staged Locations.cursedTemple
      for_ [a, b] \lid -> do
        run $ PlaceTokens GameSource (toTarget lid) Progress 1
        run $ LookAtFacedownSide pid lid
      run ShuffleUnchartedLocations
      ls <- getsGame (map toAttrs . toList . (.entities.locations))
      sort (map locationId ls) `shouldBe` sort [a, b]
      map locationCardCode ls `shouldBe` replicate 2 (toCardCode Locations.lostIsland)
      sort (mapMaybe (.otherSide) ls) `shouldBe` sort (map toCardCode [Locations.lushJungle, Locations.cursedTemple])
      map (.progress) ls `shouldBe` [0, 0]
      map (.lookedAt) ls `shouldBe` [[], []]
      filter (`elem` [jungle.id, temple.id]) (map locationCardId ls) `shouldBe` []

  describe "The Stormcaller Elite" do
    it "flips Captain Sahír from his enemy side to his objective-ally side under the first player, removing his tokens" . gameTest $ \pid -> do
      card <- genEncounterCard Enemies.captainSahir
      run $ AddToStaging card
      eid <- fromJustNote "no Sahír" <$> enemyWithCode Enemies.captainSahir
      run $ PlaceTokens GameSource (toTarget eid) Resource 3
      run $ Flip (toTarget eid)
      enemyWithCode Enemies.captainSahir `shouldReturn` Nothing
      aid <- fromJustNote "no ally Sahír" <$> allyWithCode Objectives.captainSahirObjective
      a <- getsGame (fmap toAttrs . find ((== aid) . toId) . toList . (.entities.allies))
      (allyPlacement <$> a) `shouldBe` Just (InPlayArea pid)
      (countTokens Resource . allyTokens <$> a) `shouldBe` Just 0
      (allyCardId <$> a) `shouldBe` Just card.id

    it "puts Na'asiyah's objective-ally side under the last player, and flips her back to her enemy side in the staging area" . gameTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Objectives.naasiyahObjective
      aid <- fromJustNote "no ally Na'asiyah" <$> allyWithCode Objectives.naasiyahObjective
      getsGame (fmap (allyPlacement . toAttrs) . find ((== aid) . toId) . toList . (.entities.allies)) `shouldReturn` Just (InPlayArea pid)
      run $ Flip (toTarget aid)
      allyWithCode Objectives.naasiyahObjective `shouldReturn` Nothing
      eid <- fromJustNote "no enemy Na'asiyah" <$> enemyWithCode Enemies.naasiyah
      getsGame (fmap (.placement) . find ((== eid) . toId) . toList . (.entities.enemies)) `shouldReturn` Just StagingArea

    it "cancels the damage Captain Sahír would take, a point for each resource discarded from him" . gameTest $ \_ -> do
      run . AddToStaging =<< genEncounterCard Enemies.captainSahir
      eid <- fromJustNote "no Sahír" <$> enemyWithCode Enemies.captainSahir
      run $ PlaceTokens GameSource (toTarget eid) Resource 2
      run $ DealDamage GameSource (toTarget eid) 3
      getDamage (toTarget eid) `shouldReturn` 1
      getsGame (fmap (countTokens Resource . enemyTokens . toAttrs) . find ((== eid) . toId) . toList . (.entities.enemies)) `shouldReturn` Just 0

    it "gives Captain Sahír's objective-ally side +1 willpower for each resource on him, and loses the game as he leaves play" . gameTest $ \_ -> do
      run . AddToStaging =<< genEncounterCard Objectives.captainSahirObjective
      aid <- fromJustNote "no ally Sahír" <$> allyWithCode Objectives.captainSahirObjective
      run $ PlaceTokens GameSource (toTarget aid) Resource 8
      getWillpower (toTarget aid) `shouldReturn` 6
      run $ Discard (toTarget aid)
      getsGame (.outcome) `shouldReturn` Just Lost
