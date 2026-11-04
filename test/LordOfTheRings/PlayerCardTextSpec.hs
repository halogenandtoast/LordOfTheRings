-- | Player cards whose text reaches past the common case.
module LordOfTheRings.PlayerCardTextSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Quest.CardDefs qualified as Quests
import TestImport

progressOf :: Target -> GameT Int
progressOf t = getsGame (maybe 0 (.progress) . find ((== t) . toTarget) . toList . (.entities.quests))

allySource :: Target -> Source
allySource = \case
  AllyTarget aid -> AllySource aid
  t -> error $ "not an ally: " <> show t

spec :: Spec
spec = describe "Player card text" do
  describe "Berelind" do
    it "places its progress on a quest card of the player's choice, a side quest among them" . gameTest $ \pid -> do
      main <- putQuestStage Quests.fliesAndSpiders
      run . AddToStaging =<< genEncounterCard Quests.scavengeForSupplies
      side <- getsGame (toTarget . fromJustNote "no side quest" . find ((== toCardCode Quests.scavengeForSupplies) . toCardCode) . toList . (.entities.quests))
      berelind <- putAllyIntoPlay pid Allies.berelind
      run $ UseCardAbility pid (allySource berelind) 1 []
      chooseTarget pid side
      progressOf side `shouldReturn` 1
      progressOf main `shouldReturn` 0
