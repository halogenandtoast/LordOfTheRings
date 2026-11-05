{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Game.Serialization () where

import LordOfTheRings.Ally ()
import LordOfTheRings.Attachment ()
import LordOfTheRings.Contract ()
import LordOfTheRings.Enemy ()
import LordOfTheRings.Entities (Entities)
import LordOfTheRings.Game.Base (Area, Game, Separation)
import LordOfTheRings.Hero ()
import LordOfTheRings.Json (aesonOptions, withDefaults)
import LordOfTheRings.Location ()
import LordOfTheRings.Modifier (Modifier)
import LordOfTheRings.Objective ()
import LordOfTheRings.Prelude hiding (first, many)
import LordOfTheRings.Quest ()
import LordOfTheRings.Scenario ()
import LordOfTheRings.Target (Target)
import LordOfTheRings.Treachery ()

instance FromJSON Entities where
  parseJSON = genericParseJSON (aesonOptions $ Just "entities") . withDefaults [("objectives", object []), ("contracts", object [])]

instance FromJSON Game where
  parseJSON =
    genericParseJSON (aesonOptions $ Just "game")
      . withDefaults [("logBlocks", toJSON ([] :: [Value])), ("leftPlay", object []), ("leftPlayLocations", object []), ("attackDestroyed", toJSON ([] :: [Value])), ("damageDealt", toJSON ([] :: [Value])), ("separation", Null), ("additionalPhases", toJSON ([] :: [Value])), ("resumeAfter", Null), ("revealer", Null), ("peril", Null), ("fortitudeTests", toJSON ([] :: [Value])), ("surgeChain", toJSON ([] :: [Value]))]

instance FromJSON Separation where
  parseJSON = genericParseJSON (aesonOptions $ Just "separation") . withDefaults [("shared", toJSON False)]

instance FromJSON Area where
  parseJSON = genericParseJSON (aesonOptions $ Just "area") . withDefaults [("heldModifiers", toJSON (mempty :: Map Target [Modifier])), ("inFocus", toJSON False)]

