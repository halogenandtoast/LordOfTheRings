{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Ally (module LordOfTheRings.Ally.Type, module LordOfTheRings.Ally) where

import Control.Monad.Random (MonadRandom (getRandom))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import LordOfTheRings.Ally.Type
import LordOfTheRings.Card
import LordOfTheRings.Card.PlayerCard qualified as PC
import LordOfTheRings.Id
import LordOfTheRings.Name
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Run

type AllyCard = PlayerCardDef 'AllyKind

instance Run Ally where
  run msg (Ally a) = Ally <$> run msg a

instance Run AllyAttrs where
  run msg attrs = case msg of
    _ -> pure attrs

instance HasField "id" Ally AllyId where
  getField (Ally a) = (allyAttrs a).id

instance HasField "sphere" Ally Sphere where
  getField (Ally a) = (allyAttrs a).sphere

newtype Beorn = Beorn AllyAttrs
  deriving newtype (Show, ToJSON)
instance IsAlly Beorn
instance Run Beorn where
  run msg (Beorn a) = Beorn <$> run msg a

newtype EreborHammersmith = EreborHammersmith AllyAttrs
  deriving newtype (Show, ToJSON)
instance IsAlly EreborHammersmith
instance Run EreborHammersmith where
  run msg (EreborHammersmith a) = EreborHammersmith <$> run msg a

newtype Faramir = Faramir AllyAttrs
  deriving newtype (Show, ToJSON)
instance IsAlly Faramir
instance Run Faramir where
  run msg (Faramir a) = Faramir <$> run msg a

newtype Gandalf = Gandalf AllyAttrs
  deriving newtype (Show, ToJSON)
instance IsAlly Gandalf
instance Run Gandalf where
  run msg (Gandalf a) = Gandalf <$> run msg a

newtype Gleowine = Gleowine AllyAttrs
  deriving newtype (Show, ToJSON)
instance IsAlly Gleowine
instance Run Gleowine where
  run msg (Gleowine a) = Gleowine <$> run msg a

newtype GuardOfTheCitadel = GuardOfTheCitadel AllyAttrs
  deriving newtype (Show, ToJSON)
instance IsAlly GuardOfTheCitadel
instance Run GuardOfTheCitadel where
  run msg (GuardOfTheCitadel a) = GuardOfTheCitadel <$> run msg a

newtype HenamarthRiversong = HenamarthRiversong AllyAttrs
  deriving newtype (Show, ToJSON)
instance IsAlly HenamarthRiversong
instance Run HenamarthRiversong where
  run msg (HenamarthRiversong a) = HenamarthRiversong <$> run msg a

newtype MinerOfTheIronHills = MinerOfTheIronHills AllyAttrs
  deriving newtype (Show, ToJSON)
instance IsAlly MinerOfTheIronHills
instance Run MinerOfTheIronHills where
  run msg (MinerOfTheIronHills a) = MinerOfTheIronHills <$> run msg a

newtype NorthernTracker = NorthernTracker AllyAttrs
  deriving newtype (Show, ToJSON)
instance IsAlly NorthernTracker
instance Run NorthernTracker where
  run msg (NorthernTracker a) = NorthernTracker <$> run msg a

newtype SilverlodeArcher = SilverlodeArcher AllyAttrs
  deriving newtype (Show, ToJSON)
instance IsAlly SilverlodeArcher
instance Run SilverlodeArcher where
  run msg (SilverlodeArcher a) = SilverlodeArcher <$> run msg a

newtype SnowbournScout = SnowbournScout AllyAttrs
  deriving newtype (Show, ToJSON)
instance IsAlly SnowbournScout
instance Run SnowbournScout where
  run msg (SnowbournScout a) = SnowbournScout <$> run msg a

newtype SonOfArnor = SonOfArnor AllyAttrs
  deriving newtype (Show, ToJSON)
instance IsAlly SonOfArnor
instance Run SonOfArnor where
  run msg (SonOfArnor a) = SonOfArnor <$> run msg a

lookupAlly :: CardCode -> AllyAttrs -> Ally
lookupAlly cCode = fromMaybe (error "No such ally") $ Map.lookup cCode allData
 where
  allData =
    Map.fromList
      [ ("01031", Ally . Beorn)
      , ("01059", Ally . EreborHammersmith)
      , ("01013", Ally . Faramir)
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
minerOfTheIronHills = ally "01061" "Miner of the Iron Hills" Lore 2 0 0 1 2

northernTracker :: AllyCard
northernTracker = ally "01045" "Nothern Tracker" Spirit 4 1 2 2 3

silverlodeArcher :: AllyCard
silverlodeArcher = ally "01017" "Silverload Archer" Leadership 3 1 2 0 1

snowbournScout :: AllyCard
snowbournScout = ally "01016" "Snowbourn Scout" Leadership 1 0 0 1 1

sonOfArnor :: AllyCard
sonOfArnor = ally "01015" "Son of Arnor" Leadership 3 0 2 0 2
