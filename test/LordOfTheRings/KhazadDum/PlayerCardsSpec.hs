module LordOfTheRings.KhazadDum.PlayerCardsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getHitPoints, getWillpower)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Trait (Trait (Orc))
import LordOfTheRings.Window (WindowType (AttackedEnemy, EnemyDestroyedByAttack), mkAfter)
import TestImport

-- Dáin Ironfoot (Leadership), Ori and Bifur (Lore): all Dwarves.
dainCode, oriCode, bifurCode :: CardCode
dainCode = "02116"
oriCode = "131004"
bifurCode = "03002"

kdTest :: (PlayerId -> GameT ()) -> IO ()
kdTest = gameTestWith (coreDeck "dwarves-of-durin")

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

-- | A stat once the modifiers in play (Dáin's among them) are counted.
stat :: (Target -> GameT Int) -> Target -> GameT Int
stat f t = run (Run []) >> f t

putDwalin :: PlayerId -> GameT Target
putDwalin pid = do
  run . PutHeroIntoPlay pid =<< genPlayerCard Heroes.dwalin (Just pid)
  hero (toCardCode Heroes.dwalin)

-- | A Wolf Rider (Orc, 2 hit points) in the staging area.
wolfRider :: GameT EnemyId
wolfRider = do
  run . AddToStaging =<< genEncounterCard Enemies.wolfRider
  getsGame (fromJustNote "no enemy" . headMay . map toId . toList . (.entities.enemies))

-- | Mountains of Mirkwood (Forest, Mountain) as the active location.
mountains :: GameT LocationId
mountains = do
  run . AddToStaging =<< genEncounterCard Locations.mountainsOfMirkwood
  lid <- getsGame (fromJustNote "no location" . headMay . map toId . toList . (.entities.locations))
  run $ TravelTo lid
  pure lid

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

spec :: Spec
spec = describe "Khazad-dûm player cards" do
  it "Dwalin lowers your threat by 2 after he destroys an Orc" . kdTest $ \pid -> do
    dwalin <- putDwalin pid
    eid <- wolfRider
    run $ RaiseThreat pid 10
    was <- threat pid
    run $ CheckWindows [mkAfter $ EnemyDestroyedByAttack eid [dwalin] (setFromList [Orc])]
    chooseAbilityOf pid dwalin
    threat pid `shouldReturn` was - 2

  it "Ever Onward keeps the chosen player's threat from rising for questing unsuccessfully" . kdTest $ \pid -> do
    _ <- wolfRider
    card <- putCardIntoHand pid Events.everOnward
    run . (\d -> PlaceTokens GameSource d Resource 3) =<< hero dainCode
    was <- threat pid
    run QuestResolution
    chooseTarget pid (CardIdTarget card.id)
    threat pid `shouldReturn` was

  it "Veteran of Nanduhirion enters play with 1 damage" . kdTest $ \pid -> do
    veteran <- putAllyIntoPlay pid Allies.veteranOfNanduhirion
    getCharacterDamage veteran `shouldReturn` 1

  it "Dwarrowdelf Axe gives +1 attack and deals 1 damage to the enemy attacked" . kdTest $ \pid -> do
    ori <- hero oriCode
    base <- stat getAttack ori
    axe <- putAttachmentOn pid Attachments.dwarrowdelfAxe ori
    getAttack ori `shouldReturn` base + 1
    eid <- wolfRider
    run $ CheckWindows [mkAfter $ AttackedEnemy ori eid]
    chooseAbilityOf pid axe
    getsGame (fmap (.damage) . find ((== eid) . toId) . toList . (.entities.enemies)) `shouldReturn` Just 1

  -- Gimli, a Tactics Dwarf, gives the 0 cost event its resource match
  it "Khazâd! Khazâd! gives a Dwarf +3 attack" . gameTestWith (coreDeck "dwarves-of-durin") {heroes = [dainCode, oriCode, "01004"]} $ \pid -> do
    ori <- hero oriCode
    base <- stat getAttack ori
    _ <- playWith pid "01004" 0 Events.khazadKhazad
    chooseTarget pid ori
    getAttack ori `shouldReturn` base + 3

  it "Untroubled by Darkness gives each Dwarf +1 willpower" . kdTest $ \pid -> do
    dwalin <- putDwalin pid
    ori <- hero oriCode
    base <- stat getWillpower ori
    dwalinBase <- getWillpower dwalin
    _ <- playWith pid (toCardCode Heroes.dwalin) 2 Events.untroubledByDarkness
    getWillpower ori `shouldReturn` base + 1
    getWillpower dwalin `shouldReturn` dwalinBase + 1

  it "Ancestral Knowledge exhausts a Dwarf for 4 progress on a Mountain" . kdTest $ \pid -> do
    lid <- mountains
    _ <- playWith pid oriCode 1 Events.ancestralKnowledge
    chooseTarget pid =<< hero bifurCode
    (.progress) <$> getLocation lid `shouldReturn` 4
    heroExhausted . toAttrs <$> getHeroWithCode bifurCode `shouldReturn` True

  it "Boots from Erebor give +1 hit point" . kdTest $ \pid -> do
    ori <- hero oriCode
    base <- getHitPoints ori
    _ <- putAttachmentOn pid Attachments.bootsFromErebor ori
    getHitPoints ori `shouldReturn` base + 1
