module LordOfTheRings.VoiceOfIsengard.SharedEncounterSetsSpec (spec) where

import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEngagementCost, getEnemyAttack, getEnemyThreat, getLocationQuestPoints, getLocationThreat)
import LordOfTheRings.Keyword (Keyword (Time))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy, getTimeCounters)
import LordOfTheRings.Modifier (ModifierType (CannotBeTraveledTo, GainKeyword))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

-- | A stage with Time X on its B side, as the scenarios of The Voice of Isengard have.
timedQuest :: Int -> GameT Target
timedQuest n = do
  q <- putQuestStage Quests.fliesAndSpiders
  run $ CreateEffect $ Effect GameSource q [GainKeyword (Time n)] UntilEndOfRound
  run $ Flip q
  pure q

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

staged :: CardDef -> GameT ()
staged def = run . AddToStaging =<< genEncounterCard def

enemiesOf :: CardDef -> GameT [EnemyId]
enemiesOf def = getsGame (map toId . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = fromJustNote "no such enemy" . headMay <$> enemiesOf def

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

treacheryPlacement :: CardDef -> GameT (Maybe Placement)
treacheryPlacement def = getsGame (fmap (.placement) . find ((== toCardCode def) . toCardCode) . toList . (.entities.treacheries))

heroTargets :: GameT [Target]
heroTargets = getsGame (map toTarget . toList . (.entities.heroes))

-- | Beravor, with 4 hit points.
firstHero :: GameT Target
firstHero = toTarget <$> getHeroWithCode "01012"

fillHand :: PlayerId -> Int -> GameT ()
fillHand pid n = replicateM_ n $ putCardIntoHand pid Enemies.forestSpider

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  staged def
  eid <- enemyOf def
  run $ EngageEnemy pid eid
  pure eid

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

prompt :: PlayerId -> GameT (Maybe Text)
prompt pid = fmap (\case QuestionLabel l _ _ _ -> l; _ -> "") <$> getQuestion pid

spec :: Spec
spec = describe "The Voice of Isengard's shared encounter sets" do
  describe "Dunland Raiders" do
    it "Dunland Prowler surges while a player holds 3 cards, and has +1 threat at 5" . gameTest $ \pid -> do
      fillHand pid 2
      reveal [Enemies.dunlandProwler, Enemies.forestSpider]
      enemiesOf Enemies.forestSpider `shouldReturn` []
      fillHand pid 1
      reveal [Enemies.dunlandProwler, Enemies.forestSpider]
      length <$> enemiesOf Enemies.forestSpider `shouldReturn` 1
      prowler <- enemyOf Enemies.dunlandProwler
      getEnemyThreat prowler `shouldReturn` 1
      fillHand pid 2
      run $ Run []
      getEnemyThreat prowler `shouldReturn` 2

    it "Dunlending Bandit gets +1 attack for each card in the engaged player's hand" . gameTest $ \pid -> do
      fillHand pid 3
      bandit <- engaged pid Enemies.dunlendingBandit
      getEnemyAttack bandit `shouldReturn` 4

    it "Dunland Raider has the engaged player divide damage equal to his hand among his characters" . gameTest $ \pid -> do
      fillHand pid 3
      _ <- engaged pid Enemies.dunlandRaider
      [a, b] <- take 2 <$> heroTargets
      chooseTarget pid a
      chooseTarget pid b
      chooseTarget pid a
      getDamage a `shouldReturn` 2
      getDamage b `shouldReturn` 1

    it "Old Hatreds attaches to the current quest, surges, and raises a drawing player's threat" . gameTest $ \pid -> do
      q <- timedQuest 3
      reveal [Treacheries.oldHatreds, Enemies.forestSpider]
      treacheryPlacement Treacheries.oldHatreds `shouldReturn` Just (AttachedTo q)
      length <$> enemiesOf Enemies.forestSpider `shouldReturn` 1
      threat <- threatOf pid
      run $ DrawCards (Drawing pid 2)
      threatOf pid `shouldReturn` threat + 1

    it "Old Hatreds is discarded as its stage leaves play" . gameTest $ \_ -> do
      QuestTarget qid <- timedQuest 3
      reveal [Treacheries.oldHatreds]
      run $ CompleteQuest qid
      treacheryPlacement Treacheries.oldHatreds `shouldReturn` Nothing

  describe "Dunland Warriors" do
    it "Dunland Tribesman has each player draw, and gets +1 threat as a player draws" . gameTest $ \pid -> do
      reveal [Enemies.dunlandTribesman]
      length . (.hand) <$> getPlayer pid `shouldReturn` 1
      tribesman <- enemyOf Enemies.dunlandTribesman
      getEnemyThreat tribesman `shouldReturn` 1

    it "Dunland Chieftain discards cards for the hand and puts the topmost Dunland enemy into play engaged" . gameTest $ \pid -> do
      fillHand pid 3
      _ <- setEncounterDeck [Enemies.forestSpider, Enemies.dunlandBerserker, Enemies.dunlandTribesman, Enemies.dunlandProwler]
      _ <- engaged pid Enemies.dunlandChieftain
      berserker <- enemyOf Enemies.dunlandBerserker
      (.placement) <$> getEnemy berserker `shouldReturn` EngagedWith pid
      enemiesOf Enemies.dunlandTribesman `shouldReturn` []
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.dunlandProwler]

    it "Dunland Berserker attacks as the engaged player draws" . gameTest $ \pid -> do
      _ <- engaged pid Enemies.dunlandBerserker
      run $ DrawCards (Drawing pid 1)
      passActions pid
      prompt pid >>= (`shouldSatisfy` maybe False ("$label.declareDefender" `isPrefixOf`))

    it "Wild Men of Dunland has a drawing player deal 1 damage to a character" . gameTest $ \pid -> do
      _ <- timedQuest 3
      reveal [Treacheries.wildMenOfDunland]
      run $ DrawCards (Drawing pid 1)
      h <- firstHero
      chooseTarget pid h
      getDamage h `shouldReturn` 1

  describe "Misty Mountain Orcs" do
    it "Orc Skirmisher has the engaged player remove a time counter from the quest instead of taking damage" . gameTest $ \pid -> do
      q <- timedQuest 3
      _ <- engaged pid Enemies.orcSkirmisher
      chooseLabel "$label.removeTimeCounterFromQuest" pid
      getTimeCounters q `shouldReturn` 2

    it "Orc Skirmisher deals 3 damage when no time counter can be removed" . gameTest $ \pid -> do
      _ <- engaged pid Enemies.orcSkirmisher
      h <- firstHero
      chooseTarget pid h
      getDamage h `shouldReturn` 3

    it "Orc Hunter reveals another card, or removes a time counter from the quest" . gameTest $ \pid -> do
      q <- timedQuest 3
      _ <- setEncounterDeck [Enemies.orcHunter, Enemies.forestSpider]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.orcHunter.reveal" pid
      length <$> enemiesOf Enemies.forestSpider `shouldReturn` 1
      _ <- setEncounterDeck [Enemies.orcHunter]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.removeTimeCounterFromQuest" pid
      getTimeCounters q `shouldReturn` 2

    it "Orc Hound surges and has the engaged player exhaust a character" . gameTest $ \pid -> do
      reveal [Enemies.orcHound, Enemies.forestSpider]
      length <$> enemiesOf Enemies.forestSpider `shouldReturn` 1
      hound <- enemyOf Enemies.orcHound
      run $ EngageEnemy pid hound
      h <- firstHero
      chooseTarget pid h
      getsGame (any (heroExhausted . toAttrs) . toList . (.entities.heroes)) `shouldReturn` True

    it "Orc Hunting Party deals Orcs shadow cards, lowers their engagement cost, and surges with none staged" . gameTest $ \pid -> do
      skirmisher <- engaged pid Enemies.orcHunter
      reveal [Treacheries.orcHuntingParty, Enemies.forestSpider, Enemies.forestSpider]
      length . (.shadowCards) <$> getEnemy skirmisher `shouldReturn` 1
      getEngagementCost skirmisher `shouldReturn` 20
      length <$> enemiesOf Enemies.forestSpider `shouldReturn` 1

  describe "Broken Lands" do
    it "keeps progress off locations in the staging area while it is there" . gameTest $ \_ -> do
      staged Locations.brokenLandsLocation
      broken <- locationOf Locations.brokenLandsLocation
      hid <- getsGame (fromJustNote "no hero" . headMay . map toId . toList . (.entities.heroes))
      run $ PlaceTokens (HeroSource hid) (toTarget broken) Progress 2
      (.progress) <$> getLocation broken `shouldReturn` 0
      run $ PlaceLocation broken ActiveLocation
      run $ PlaceTokens GameSource (toTarget broken) Progress 2
      (.progress) <$> getLocation broken `shouldReturn` 2

    it "Prowling Wolf gets +1 attack for each location in the staging area while engaged" . gameTest $ \pid -> do
      staged Locations.brokenLandsLocation
      staged Locations.tangledWoods
      wolf <- engaged pid Enemies.prowlingWolf
      getEnemyAttack wolf `shouldReturn` 3

    it "Take Cover! deals 1 damage to each exhausted character, or removes a time counter" . gameTest $ \pid -> do
      q <- timedQuest 3
      h <- firstHero
      run $ Exhaust h
      reveal [Treacheries.takeCover, Enemies.forestSpider]
      chooseLabel "$label.takeCover.damage" pid
      getDamage h `shouldReturn` 1
      getTimeCounters q `shouldReturn` 3

  describe "Ancient Forest" do
    it "gives each Forest in the staging area +1 threat and +3 quest points, not stacking" . gameTest $ \_ -> do
      staged Locations.ancientForestLocation
      staged Locations.ancientForestLocation
      staged Locations.tangledWoods
      woods <- locationOf Locations.tangledWoods
      getLocationThreat woods `shouldReturn` 3
      getLocationQuestPoints woods `shouldReturn` 7

    it "Tangled Woods makes the players exhaust a hero to travel to a Forest" . gameTest $ \pid -> do
      staged Locations.tangledWoods
      staged Locations.ancientForestLocation
      forest <- locationOf Locations.ancientForestLocation
      run $ TravelTo forest
      h <- firstHero
      chooseTarget pid h
      getsGame (any (heroExhausted . toAttrs) . toList . (.entities.heroes)) `shouldReturn` True
      woods <- locationOf Locations.tangledWoods
      traverse_ (run . Exhaust) =<< heroTargets
      getModifiers woods >>= (`shouldSatisfy` elem CannotBeTraveledTo)

    it "Turned Around returns the active location to the staging area" . gameTest $ \pid -> do
      _ <- timedQuest 3
      staged Locations.tangledWoods
      woods <- locationOf Locations.tangledWoods
      run $ PlaceLocation woods ActiveLocation
      reveal [Treacheries.turnedAround]
      chooseLabel "$label.turnedAround.returnLocation" pid
      (.placement) <$> getLocation woods `shouldReturn` StagingArea

  describe "Weary Travelers" do
    it "Low on Provisions has each player divide damage equal to his characters among them" . gameTest $ \pid -> do
      reveal [Treacheries.lowOnProvisions]
      h <- firstHero
      replicateM_ 3 $ chooseTarget pid h
      getDamage h `shouldReturn` 3

    it "Off Track attaches to a location for +2 quest points and removes another time counter each refresh phase" . gameTest $ \_ -> do
      q <- timedQuest 4
      staged Locations.tangledWoods
      woods <- locationOf Locations.tangledWoods
      reveal [Treacheries.offTrack, Enemies.forestSpider]
      treacheryPlacement Treacheries.offTrack `shouldReturn` Just (AttachedTo $ toTarget woods)
      getLocationQuestPoints woods `shouldReturn` 6
      run $ EndPhase RefreshPhase
      getTimeCounters q `shouldReturn` 2

    it "In Need of Rest takes a hero off the quest and damages it as time counters leave the quest" . gameTest $ \_ -> do
      q <- timedQuest 3
      h <- firstHero
      run $ CommitToQuest h
      reveal [Treacheries.inNeedOfRest]
      treacheryPlacement Treacheries.inNeedOfRest `shouldReturn` Just (AttachedTo h)
      getsGame (any (heroCommitted . toAttrs) . toList . (.entities.heroes)) `shouldReturn` False
      run $ RemoveTokens GameSource q TimeCounter 1
      getDamage h `shouldReturn` 1
