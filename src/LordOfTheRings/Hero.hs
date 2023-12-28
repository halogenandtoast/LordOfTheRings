{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Hero (module LordOfTheRings.Hero.Type) where

import GHC.Records
import LordOfTheRings.Card
import LordOfTheRings.Hero.Type
import LordOfTheRings.Message
import LordOfTheRings.Run

instance Run Hero where
  run msg (Hero a) = Hero <$> run msg a

instance Run HeroAttrs where
  run msg attrs = case msg of
    BeginPhase ResourcePhase -> do
      pure $ attrs {resources = attrs.resources + 1}
    _ -> pure attrs

instance HasField "threat" HeroAttrs Int where
  getField a = a.card.def.details.threat

instance HasField "threat" Hero Int where
  getField (Hero a) = (heroAttrs a).threat
