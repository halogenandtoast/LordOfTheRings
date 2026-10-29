module LordOfTheRings.ShadowsOfMirkwood.PlayerCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (CannotHaveRestrictedAttachments, DamageRedirectedTo))
import LordOfTheRings.Phase
import LordOfTheRings.Scenario (conflictAtTheCarrock, theHuntForGollum)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

-- | Beravor (lore), Éowyn (spirit), Théodred (leadership).
beravor, eowyn, theodred :: GameT Target
beravor = hero "01012"
eowyn = hero "01007"
theodred = hero "01002"

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  run . AddToStaging =<< genEncounterCard def
  eid <- getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))
  run $ EngageEnemy pid eid
  pure eid

heroInPlay :: Target -> GameT Bool
heroInPlay t = getsGame \g -> any ((== t) . toTarget) (toList g.entities.heroes)

spec :: Spec
spec = describe "Shadows of Mirkwood player cards" do
  describe "A Burning Brand" do
    it "cancels the shadow effects of the attack its character defends, without exhausting" . gameTest $ \pid -> do
      b <- beravor
      _ <- putAttachmentOn pid Attachments.aBurningBrand b
      eid <- engagedEnemy pid Enemies.dolGuldurOrcs
      _ <- setEncounterDeck [Enemies.dolGuldurOrcs]
      run $ DealShadowCard eid
      run $ DefendWith pid eid b
      passActions pid
      passActions pid
      -- the orcs' 2 attack, not raised by the canceled shadow, against her 2 defense
      getCharacterDamage b `shouldReturn` 0
      isJust <$> getQuestion pid `shouldReturn` False

    it "can go on a hero Song of Wisdom gives a lore icon" . gameTest $ \pid -> do
      e <- eowyn
      (e `elem`) <$> cardHosts (toCardCode Attachments.aBurningBrand) `shouldReturn` False
      _ <- putAttachmentOn pid Attachments.songOfWisdom e
      (e `elem`) <$> cardHosts (toCardCode Attachments.aBurningBrand) `shouldReturn` True

  describe "Landroval" do
    it "is limited once per game for each player across every copy" . gameTest $ \pid -> do
      firstCopy <- putAllyIntoPlay pid Allies.landroval
      run . flip (DealDamage GameSource) 4 =<< eowyn
      chooseAbilityOf pid firstCopy
      secondCopy <- putAllyIntoPlay pid Allies.landroval
      run . flip (DealDamage GameSource) 4 =<< theodred
      offersAbilityOf pid secondCopy `shouldReturn` False

  describe "Bilbo Baggins" do
    it "draws his additional card in the same draw, so Iron Shackles stops both" . gameTestIn theHuntForGollum bilboDeck $ \pid -> do
      _ <- setEncounterDeck [Treacheries.ironShackles]
      run RevealEncounterCard
      resolveRevealed
      hand <- length . (.hand) <$> getPlayer pid
      run $ BeginPhase ResourcePhase
      length . (.hand) <$> getPlayer pid `shouldReturn` hand
      getsGame (length . (.entities.treacheries)) `shouldReturn` 0

  describe "Strider's Path" do
    it "leaves an active location immune to player card effects active" . gameTestIn conflictAtTheCarrock testDeck $ \pid -> do
      run . AddToStaging =<< genEncounterCard Locations.theCarrock
      carrock <- getsGame (fromJustNote "no Carrock" . headMay . map toId . toList . (.entities.locations))
      run $ PlaceLocation carrock ActiveLocation
      run . (\b -> PlaceTokens GameSource b Resource 1) =<< beravor
      card <- putCardIntoHand pid Events.stridersPath
      _ <- setEncounterDeck [Locations.oakWoodGrove]
      run RevealEncounterCard
      chooseTarget pid (CardIdTarget card.id)
      getsGame (map (.placement) . toList . (.entities.locations)) `shouldReturn` [ActiveLocation, ActiveLocation]

  describe "Song of Mocking" do
    it "does not redirect damage again once it has been redirected" . gameTest $ \_ -> do
      e <- eowyn
      t <- theodred
      run $ CreateEffect $ Effect GameSource e [DamageRedirectedTo t] UntilEndOfPhase
      run $ CreateEffect $ Effect GameSource t [DamageRedirectedTo e] UntilEndOfPhase
      run $ DealDamage GameSource e 1
      getCharacterDamage t `shouldReturn` 1
      getCharacterDamage e `shouldReturn` 0

  describe "Frodo Baggins" do
    it "is not discarded by Muck Adder when he cancels its damage" . gameTestIn conflictAtTheCarrock frodoDeck $ \pid -> do
      frodo <- hero (toCardCode Heroes.frodoBaggins)
      _ <- setEncounterDeck [Enemies.muckAdder]
      run RevealEncounterCard
      adder <- getsGame (fromJustNote "no adder" . headMay . map toId . toList . (.entities.enemies))
      run $ DealDamage (EnemySource adder) frodo 1
      chooseAbilityOf pid frodo
      heroInPlay frodo `shouldReturn` True
      getCharacterDamage frodo `shouldReturn` 0

    it "cannot be assigned more of a divided damage than his remaining hit points" . gameTestWith frodoDeck $ \pid -> do
      frodo <- hero (toCardCode Heroes.frodoBaggins)
      run $ DealDamageDivided GameSource pid 5 []
      -- Frodo's 2 hit points
      replicateM_ 2 $ chooseTarget pid frodo
      offered <- maybe False (any \case TargetLabel t _ -> t == frodo; _ -> False) . fmap questionChoices <$> getQuestion pid
      offered `shouldBe` False

  describe "Eagles of the Misty Mountains" do
    it "cannot have restricted attachments" . gameTest $ \pid -> do
      eagles <- putAllyIntoPlay pid Allies.eaglesOfTheMistyMountains
      elem CannotHaveRestrictedAttachments <$> getModifiers eagles `shouldReturn` True
 where
  bilboDeck = DeckList [toCardCode Heroes.bilboBaggins, "01007", "01002"] testDeck.cards
  frodoDeck = DeckList [toCardCode Heroes.frodoBaggins, "01007", "01002"] testDeck.cards
