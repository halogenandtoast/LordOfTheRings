{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Ally (module LordOfTheRings.Ally.Type, module LordOfTheRings.Ally) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ability
import LordOfTheRings.Ally.Type
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Card.PlayerCard qualified as PC
import LordOfTheRings.Id
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Name
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage

type AllyCard = PlayerCardDef 'AllyKind

instance RunMessage Ally where
  runMessage msg (Ally a) = Ally <$> runMessage msg a

instance RunMessage AllyAttrs where
  runMessage msg attrs = case msg of
    _ -> pure attrs

instance HasField "id" Ally AllyId where
  getField (Ally a) = (toAttrs a).id

instance HasField "sphere" Ally Sphere where
  getField (Ally a) = (toAttrs a).sphere

newtype Beorn = Beorn AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage Beorn where
  runMessage msg (Beorn a) = Beorn <$> runMessage msg a

newtype EreborHammersmith = EreborHammersmith AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage EreborHammersmith where
  runMessage msg (EreborHammersmith a) = EreborHammersmith <$> runMessage msg a

newtype Faramir = Faramir AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage Faramir where
  runMessage msg (Faramir a) = Faramir <$> runMessage msg a

newtype Gandalf = Gandalf AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage Gandalf where
  runMessage msg (Gandalf a) = Gandalf <$> runMessage msg a

newtype Gleowine = Gleowine AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage Gleowine where
  runMessage msg (Gleowine a) = Gleowine <$> runMessage msg a

newtype GuardOfTheCitadel = GuardOfTheCitadel AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage GuardOfTheCitadel where
  runMessage msg (GuardOfTheCitadel a) = GuardOfTheCitadel <$> runMessage msg a

newtype HenamarthRiversong = HenamarthRiversong AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage HenamarthRiversong where
  runMessage msg (HenamarthRiversong a) = HenamarthRiversong <$> runMessage msg a

newtype MinerOfTheIronHills = MinerOfTheIronHills AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage MinerOfTheIronHills where
  runMessage msg (MinerOfTheIronHills a) = MinerOfTheIronHills <$> runMessage msg a

newtype NorthernTracker = NorthernTracker AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage NorthernTracker where
  runMessage msg (NorthernTracker a) = NorthernTracker <$> runMessage msg a

newtype SilverlodeArcher = SilverlodeArcher AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage SilverlodeArcher where
  runMessage msg (SilverlodeArcher a) = SilverlodeArcher <$> runMessage msg a

newtype SnowbournScout = SnowbournScout AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage SnowbournScout where
  runMessage msg s@(SnowbournScout a) = case msg of
    UseCardAbility p (isSource a -> True) 1 -> do
      locations <- select CanProgressLocation
      push $
        chooseOrRunOne p [targetChoice location [PlaceProgress (toTarget location) 1] | location <- locations]
      pure s
    _ -> SnowbournScout <$> runMessage msg a

instance HasAbilities SnowbournScout where
  getAbilities (SnowbournScout a) =
    [ response
        a
        1
        (after $ entersPlay a)
        (exists CanProgressLocation)
        "_Response_: After Snowbourn Scout enters play, choose a location. Place 1 progress token on that location."
    ]

newtype SonOfArnor = SonOfArnor AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage SonOfArnor where
  runMessage msg (SonOfArnor a) = SonOfArnor <$> runMessage msg a

lookupAlly :: CardCode -> AllyAttrs -> Ally
lookupAlly cCode = fromMaybe (error "No such ally") $ Map.lookup cCode allData
 where
  allData =
    Map.fromList
      [ ("01031", Ally . Beorn)
      , ("01059", Ally . EreborHammersmith)
      , ("01014", Ally . Faramir)
      , ("01073", Ally . Gandalf)
      , ("01062", Ally . Gleowine)
      , ("01013", Ally . GuardOfTheCitadel)
      , ("01060", Ally . HenamarthRiversong)
      , ("01061", Ally . MinerOfTheIronHills)
      , ("01045", Ally . NorthernTracker)
      , ("01017", Ally . SilverlodeArcher)
      , ("01016", Ally . SnowbournScout)
      , ("01015", Ally . SonOfArnor)
      ]

newAlly :: (MonadRandom m) => PlayerCard 'AllyKind -> m Ally
newAlly card = do
  allyId <- getRandom
  let attrs = AllyAttrs {id = allyId, card, sphere = card.def.sphere, placement = Unplaced}
  pure $ lookupAlly card.def.code attrs

allAllies :: Map CardCode (AllyCard)
allAllies =
  Map.fromList
    [ ("01013", guardOfTheCitadel)
    , ("01014", faramir)
    , ("01015", sonOfArnor)
    , ("01016", snowbournScout)
    , ("01017", silverlodeArcher)
    , ("01031", beorn)
    , ("01045", northernTracker)
    , ("01059", ereborHammersmith)
    , ("01060", henamarthRiversong)
    , ("01061", minerOfTheIronHills)
    , ("01062", gleowine)
    , ("01073", gandalf)
    ]

ally :: CardCode -> Name -> Sphere -> Int -> Int -> Int -> Int -> Int -> AllyCard
ally code name sphere cost willpower attack defense hitPoints =
  PlayerCardDef
    { code
    , name
    , PC.unique = False
    , kind = SAllyKind
    , details = AllyDetails (StaticCost cost) willpower attack defense hitPoints
    , sphere
    }

beorn :: AllyCard
beorn = unique $ ally "01031" "Beorn" Tactics 6 1 3 3 6

ereborHammersmith :: AllyCard
ereborHammersmith = ally "01059" "Erebor Hammersmith" Lore 2 1 1 1 3

faramir :: AllyCard
faramir = unique $ ally "01014" "Faramir" Leadership 4 2 1 2 3

gandalf :: AllyCard
gandalf = unique $ ally "01073" "Gandalf" Neutral 5 4 4 4 4

gleowine :: AllyCard
gleowine = unique $ ally "01062" "Gleowine" Lore 2 1 0 0 2

guardOfTheCitadel :: AllyCard
guardOfTheCitadel = ally "01013" "Guard of the Citadel" Leadership 2 1 1 0 2

henamarthRiversong :: AllyCard
henamarthRiversong = unique $ ally "01060" "Henamarth Riversong" Lore 1 1 1 0 1

minerOfTheIronHills :: AllyCard
minerOfTheIronHills = ally "01061" "Miner of the Iron Hills" Lore 2 0 1 1 2

northernTracker :: AllyCard
northernTracker = ally "01045" "Northern Tracker" Spirit 4 1 2 2 3

silverlodeArcher :: AllyCard
silverlodeArcher = ally "01017" "Silverlode Archer" Leadership 3 1 2 0 1

snowbournScout :: AllyCard
snowbournScout = ally "01016" "Snowbourn Scout" Leadership 1 0 0 1 1

sonOfArnor :: AllyCard
sonOfArnor = ally "01015" "Son of Arnor" Leadership 3 0 2 0 2
