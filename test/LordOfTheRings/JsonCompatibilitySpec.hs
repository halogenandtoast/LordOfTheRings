{-# OPTIONS_GHC -O0 #-}
-- The Generic migration oracle intentionally exercises the old expensive codecs.

module LordOfTheRings.JsonCompatibilitySpec (spec) where

import Data.Aeson (Result (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.UUID qualified as UUID
import LordOfTheRings.Json (aesonOptions)
import LordOfTheRings.Modifier qualified as M
import LordOfTheRings.Phase
import LordOfTheRings.Window qualified as W
import TestImport

-- These are the old codecs, kept independent of the generated instances. Check
-- both migration directions and toEncoding as well as toJSON (TH generates both).
compatible
  :: (Eq a, Show a, ToJSON a, FromJSON a) => (a -> Value) -> (Value -> Parser a) -> a -> Expectation
compatible oldEncode oldDecode value = do
  toJSON value `shouldBe` oldEncode value
  fromJSON (oldEncode value) `shouldBe` Success value
  parseEither oldDecode (toJSON value) `shouldBe` Right value
  eitherDecode (encode value) `shouldBe` Right value
  (eitherDecode (encode value) :: Either String Value) `shouldBe` Right (oldEncode value)

dropFields :: [Key] -> Value -> Value
dropFields fieldsToDrop = \case
  Object fields -> Object $ foldr KeyMap.delete fields fieldsToDrop
  value -> value

spec :: Spec
spec = describe "Generic codec compatibility" do
  let pid = PlayerId UUID.nil
      eid = EnemyId UUID.nil
      target = PlayerTarget pid
      escape = EscapeTest pid GameSource 2 EscapeDefense [target] [BeginRound] [EndRound] [pid]
      fortitude = FortitudeTest pid GameSource [] 2 1 [] [] [BeginRound] [EndRound]
  it "loads a saved Discard from before it named its source as the game's" do
    let old = object ["tag" .= ("Discard" :: Text), "contents" .= target]
    (fromJSON old :: Result Message) `shouldBe` Success (Discard GameSource target)
    (fromJSON (object ["tag" .= ("Run" :: Text), "contents" .= [old]]) :: Result Message) `shouldBe` Success (Run [Discard GameSource target])
    let new = Discard (EnemySource eid) target
    (fromJSON (toJSON new) :: Result Message) `shouldBe` Success new :: Expectation

  it "preserves nullary tags, positional contents, recursive messages and questions" do
    mapM_
      (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions))
      [ Setup
      , BeginPhase PlanningPhase
      , SetRevealer Nothing
      , SetRevealer (Just pid)
      , DrawCards (DrawingFrom pid 2 $ PlayerSource pid)
      , Run [BeginRound, DoStep 2 (Run [EndRound])]
      , Ask pid
          $ QuestionLabel "quoted \"日本語\"" (Just target) [target]
          $ ChooseOne [Label "continue" [MakeEscapeTest escape, MakeFortitudeTest fortitude]]
      , ScenarioSpecific "payload" (object ["nested" .= [Null, Bool True], "text" .= ("日本語\\\"" :: Text)])
      ]
  it "preserves modifier tags, nested modifiers, and the modifier record's field names" do
    let modifiers =
          [ M.ThreatHalved
          , M.WillpowerModifier (-2)
          , M.EngagementCostAgainst pid 3
          , M.ForNextAttack (M.AttackModifier 4)
          ]
    mapM_ (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions)) modifiers
    mapM_
      (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions))
      [M.Modifier GameSource m | m <- modifiers]
  it "preserves window records, sum contents and time-of-day strings" do
    let windows =
          [W.PhaseBegins PlanningPhase, W.EntersPlay target, W.EnemyEngaged eid pid, W.DealtDamage target 0]
    mapM_ (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions)) windows
    mapM_
      (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions))
      [W.Window timing window | timing <- [W.When, W.After], window <- windows]
    mapM_
      (compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions))
      [W.Daytime, W.Nighttime]
  it "preserves test records and the escape parser's legacy party default" do
    compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions) escape
    compatible (genericToJSON defaultOptions) (genericParseJSON defaultOptions) fortitude
    fromJSON (dropFields ["escapeParty"] $ toJSON escape) `shouldBe` Success escape {escapeParty = []}
  it "preserves the old drawing-source default inside saved messages" do
    let drawing = Drawing pid 2
    fromJSON (dropFields ["source"] $ toJSON drawing) `shouldBe` Success drawing
    let legacy = object ["tag" .= ("DrawCards" :: Text), "contents" .= dropFields ["source"] (toJSON drawing)]
    fromJSON legacy `shouldBe` Success (DrawCards drawing)
    pure () :: Expectation
  it "preserves live game field names and the old parser's optional-field defaults" . gameTest $ \pid' -> do
    run Setup
    game <- getGame
    let old = genericToJSON (aesonOptions $ Just "game") game
    toJSON game `shouldBe` old
    case parseEither (genericParseJSON $ aesonOptions $ Just "game") (toJSON game) of
      Left err -> liftIO $ expectationFailure err
      Right (decoded :: Game) -> toJSON decoded `shouldBe` old
    -- Fields introduced after the first saved-game schema. Use an initial game
    -- where each default is the expected value, rather than dropping live state.
    player <- getPlayer pid'
    let initial = newGame player []
        legacy =
          dropFields
            [ "logBlocks"
            , "leftPlay"
            , "leftPlayLocations"
            , "attackDestroyed"
            , "damageDealt"
            , "separation"
            , "additionalPhases"
            , "resumeAfter"
            , "revealer"
            , "peril"
            , "fortitudeTests"
            , "surgeChain"
            ]
            (toJSON initial)
    case fromJSON legacy of
      Error err -> liftIO $ expectationFailure err
      Success (decoded :: Game) -> toJSON decoded `shouldBe` toJSON initial
