{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Ally (module LordOfTheRings.Ally.Type, module LordOfTheRings.Ally) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ability
import LordOfTheRings.Ally.Type
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
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
        chooseOrRunOne p [targetLabel location [PlaceProgress (toTarget location) 1] | location <- locations]
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

newAlly :: (MonadRandom m) => PlayerCard -> m Ally
newAlly card = do
  allyId <- getRandom
  let attrs = AllyAttrs {id = allyId, card, sphere = fromMaybe Neutral $ cdSphere (toCardDef card), placement = Unplaced}
  pure $ lookupAlly (toCardCode card) attrs
