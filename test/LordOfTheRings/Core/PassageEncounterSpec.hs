module LordOfTheRings.Core.PassageEncounterSpec (spec) where

import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

-- | Reveal a card from the encounter deck, with the given cards under it.
reveal :: CardDef -> [CardDef] -> GameT ()
reveal def rest = setEncounterDeck (def : rest) >> run RevealEncounterCard

theLocation :: GameT LocationId
theLocation = getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))

theEnemy :: GameT EnemyId
theEnemy = getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))

exhausted :: CardCode -> GameT Bool
exhausted code = heroExhausted . toAttrs <$> getHeroWithCode code

spec :: Spec
spec = do
  describe "Great Forest Web" do
    it "cannot be travelled to while a player has no ready hero" . gameTest $ \pid -> do
      reveal Locations.greatForestWeb []
      for_ ["01012", "01007", "01002"] \code -> run . Exhaust =<< hero code
      run $ BeginPhase TravelPhase
      getQuestion pid `shouldReturn` Nothing

    it "makes each player exhaust a hero to travel there" . gameTest $ \pid -> do
      reveal Locations.greatForestWeb []
      run . TravelTo =<< theLocation
      chooseTarget pid =<< hero "01007"
      exhausted "01007" `shouldReturn` True

  describe "Mountains of Mirkwood" do
    it "reveals the top encounter card to travel there" . gameTest $ \_ -> do
      reveal Locations.mountainsOfMirkwood [Locations.forestGate]
      run . TravelTo =<< theLocation
      getsGame (length . (.entities.locations)) `shouldReturn` 2

    it "lets each player search the top 5 cards of their deck when explored" . gameTest $ \pid -> do
      reveal Locations.mountainsOfMirkwood []
      lid <- theLocation
      run $ PlaceTokens GameSource (toTarget lid) Progress 3
      chooseAbilityOf pid lid
      top <- fromJustNote "empty deck" . headMay . (.deck) <$> getPlayer pid
      chooseTarget pid (CardIdTarget top.id)
      map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem top.id)

  describe "Old Forest Road" do
    it "lets the first player ready a character after travelling there" . gameTest $ \pid -> do
      reveal Locations.oldForestRoad []
      eowyn <- hero "01007"
      run $ Exhaust eowyn
      lid <- theLocation
      run $ TravelTo lid
      chooseAbilityOf pid lid
      chooseTarget pid eowyn
      exhausted "01007" `shouldReturn` False

  describe "Forest Gate" do
    it "lets the first player draw 2 cards after travelling there" . gameTest $ \pid -> do
      reveal Locations.forestGate []
      lid <- theLocation
      before <- handSize pid
      run $ TravelTo lid
      chooseAbilityOf pid lid
      handSize pid `shouldReturn` before + 2

  describe "Necromancer's Pass" do
    it "makes the first player discard 2 random cards to travel there" . gameTest $ \pid -> do
      reveal Locations.necromancersPass []
      _ <- putCardIntoHand pid Events.grimResolve
      run $ BeginPhase TravelPhase
      getQuestion pid `shouldReturn` Nothing
      _ <- putCardIntoHand pid Events.grimResolve
      run . TravelTo =<< theLocation
      handSize pid `shouldReturn` 0

  describe "Enchanted Stream" do
    it "stops players drawing while it is the active location" . gameTest $ \pid -> do
      reveal Locations.enchantedStream []
      run . TravelTo =<< theLocation
      before <- handSize pid
      run $ DrawCards $ Drawing pid 1
      handSize pid `shouldReturn` before

  describe "Black Forest Bats" do
    it "removes a committed character from the quest" . gameTest $ \pid -> do
      eowyn <- hero "01007"
      run $ CommitToQuest eowyn
      reveal Enemies.blackForestBats []
      chooseTarget pid eowyn
      heroCommitted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` False
      exhausted "01007" `shouldReturn` True

  describe "Dol Guldur Orcs" do
    it "deals 2 damage to a committed character when revealed" . gameTest $ \pid -> do
      eowyn <- hero "01007"
      run $ CommitToQuest eowyn
      reveal Enemies.dolGuldurOrcs []
      chooseTarget pid eowyn
      getCharacterDamage eowyn `shouldReturn` 2

    it "as a shadow gives the attacker +3 attack when undefended" . gameTest $ \pid -> do
      reveal Enemies.hummerhorns []
      eid <- theEnemy
      [shadow] <- traverse genEncounterCard [Enemies.dolGuldurOrcs]
      run $ AttachShadowCard eid shadow
      run $ ResolveShadow eid pid Nothing
      getEnemyAttack eid `shouldReturn` 5

  describe "East Bight Patrol" do
    it "as a shadow raises the defending player's threat by 3 when undefended" . gameTest $ \pid -> do
      reveal Enemies.hummerhorns []
      eid <- theEnemy
      threat <- (.threat) <$> getPlayer pid
      [shadow] <- traverse genEncounterCard [Enemies.eastBightPatrol]
      run $ AttachShadowCard eid shadow
      run $ ResolveShadow eid pid Nothing
      (.threat) <$> getPlayer pid `shouldReturn` threat + 3
      getEnemyAttack eid `shouldReturn` 3

  describe "Chieftain Ufthak" do
    it "gains a resource after attacking, and +2 attack for each" . gameTest $ \pid -> do
      reveal Enemies.chieftanUfthak []
      eid <- theEnemy
      run $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
      getEnemyAttack eid `shouldReturn` 5

  describe "Dol Guldur Beastmaster" do
    it "is dealt an additional shadow card when it attacks" . gameTest $ \pid -> do
      reveal Enemies.dolGuldurBeastmaster [Locations.forestGate]
      eid <- theEnemy
      run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
      getsGame (maybe 0 (length . enemyShadowCards . toAttrs) . lookup eid . mapToList . (.entities.enemies)) `shouldReturn` 1

  describe "Caught in a Web" do
    it "keeps the hero exhausted in the refresh phase unless 2 of its resources are paid" . gameTest $ \pid -> do
      reveal Treacheries.caughtInAWeb []
      eowyn <- hero "01007"
      chooseTarget pid eowyn
      run $ Exhaust eowyn
      run $ BeginPhase RefreshPhase
      exhausted "01007" `shouldReturn` True
      run $ PlaceTokens GameSource eowyn Resource 2
      run $ CheckWindows [mkAfter $ PhaseBegins RefreshPhase]
      web <- getsGame (toTarget . fromJustNote "no web" . headMay . toList . (.entities.treacheries))
      chooseAbilityOf pid web
      exhausted "01007" `shouldReturn` False
