module LordOfTheRings.RingMaker.PlayerCardsSpec (spec) where

import LordOfTheRings.Event.CardDefs qualified as Events
import TestImport

beravorCode, theodredCode :: CardCode
beravorCode = "01012"
theodredCode = "01002"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

setThreat :: PlayerId -> Int -> GameT ()
setThreat pid n = putGame . (playersL . ix pid . threatL .~ n) =<< getGame

isExhausted :: Target -> GameT Bool
isExhausted = \case
  HeroTarget hid -> heroExhausted . toAttrs <$> getsGame (fromJustNote "hero" . lookup hid . mapToList . (.entities.heroes))
  AllyTarget aid -> maybe False (allyExhausted . toAttrs) . lookup aid . mapToList <$> getsGame (.entities.allies)
  _ -> pure False

inHand :: PlayerId -> CardId -> GameT Bool
inHand pid cid = elem cid . map (.id) . (.hand) <$> getPlayer pid

spec :: Spec
spec = describe "The Ring-maker cycle player cards" do
  describe "Swift and Silent" do
    it "returns to hand the first time it is played in a round, while threat is 20 or less" . gameTest $ \pid -> do
      setThreat pid 20
      beravor <- hero beravorCode
      run $ Exhaust beravor
      card <- playWith pid theodredCode 2 Events.swiftAndSilent
      chooseTarget pid beravor
      isExhausted beravor `shouldReturn` False
      inHand pid card.id `shouldReturn` True
      run $ Exhaust beravor
      chooseTarget pid (CardIdTarget card.id)
      chooseTarget pid beravor
      inHand pid card.id `shouldReturn` False

    it "is discarded while threat is above 20" . gameTest $ \pid -> do
      setThreat pid 21
      card <- playWith pid theodredCode 1 Events.swiftAndSilent
      chooseTarget pid =<< hero beravorCode
      inHand pid card.id `shouldReturn` False
