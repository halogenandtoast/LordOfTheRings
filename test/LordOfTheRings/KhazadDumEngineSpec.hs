module LordOfTheRings.KhazadDumEngineSpec (spec) where

import LordOfTheRings.Card (EncounterCard (..))
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Message.Lifted (cancelDamage)
import LordOfTheRings.Modifier (ModifierType (CannotTakeDamage))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import TestImport

spec :: Spec
spec = describe "Khazad-dûm engine" do
  it "cancels damage from the window before it is dealt" . gameTest $ \_ -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    runOnly $ DealDamage GameSource beravor 2
    runQueueT $ cancelDamage beravor
    void mainLoop
    getDamage beravor `shouldReturn` 0

  it "deals no damage to a character that takes none" . gameTest $ \_ -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    run $ CreateEffect $ Effect GameSource beravor [CannotTakeDamage] UntilEndOfRound
    run $ DealDamage GameSource beravor 2
    getDamage beravor `shouldReturn` 0

  it "discards the top of the encounter deck for the card to read back" . gameTest $ \_ -> do
    [spider, _] <- setEncounterDeck [Enemies.forestSpider, Enemies.forestSpider]
    run $ DiscardTopOfEncounterDeck GameSource GameTarget 1
    getEncounterDiscard `shouldReturn` [spider]
    length <$> getEncounterDeck `shouldReturn` 1

  it "shuffles the stage in play back into the quest deck" . gameTest $ \_ -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    quest <- getActiveQuest
    run $ ShuffleIntoQuestDeck $ MkEncounterCard (toAttrs quest).questCardId (toCardCode quest)
    getsGame (length . (.entities.quests)) `shouldReturn` 0
    getsGame (map toCardCode . (.questDeck) . toAttrs . (.scenario)) `shouldReturn` [toCardCode Quests.fliesAndSpiders]
