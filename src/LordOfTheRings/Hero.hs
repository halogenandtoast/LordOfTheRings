{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Hero (module LordOfTheRings.Hero.Type, module LordOfTheRings.Hero) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Hero.Type
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Name
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage

instance RunMessage Hero where
  runMessage msg (Hero a) = Hero <$> runMessage msg a

instance RunMessage HeroAttrs where
  runMessage msg attrs = case msg of
    BeginPhase ResourcePhase -> do
      push $ GainHeroResources attrs.id 1
      pure attrs
    GainHeroResources hId n | hId == attrs.id -> do
      pure $ attrs {resources = attrs.resources + n}
    SpendHeroResources hId n | hId == attrs.id -> do
      pure $ attrs {resources = max 0 (attrs.resources - n)}
    _ -> pure attrs

newtype Beravor = Beravor HeroAttrs
  deriving anyclass (IsHero)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage Beravor where
  runMessage msg (Beravor a) = Beravor <$> runMessage msg a

newtype Eowyn = Eowyn HeroAttrs
  deriving anyclass (IsHero)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage Eowyn where
  runMessage msg (Eowyn a) = Eowyn <$> runMessage msg a

newtype Theodred = Theodred HeroAttrs
  deriving anyclass (IsHero)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage Theodred where
  runMessage msg (Theodred a) = Theodred <$> runMessage msg a

lookupHero :: CardCode -> HeroAttrs -> Hero
lookupHero cCode = fromMaybe (error "No such hero") $ Map.lookup cCode allData
 where
  allData = Map.fromList [("01012", Hero . Beravor), ("01007", Hero . Eowyn), ("01002", Hero . Theodred)]

newHero :: (MonadRandom m) => PlayerId -> PlayerCard -> m Hero
newHero player card = do
  heroId <- getRandom
  let attrs = HeroAttrs {id = heroId, card, player, resources = 0, sphere = fromMaybe Neutral $ cdSphere (toCardDef card)}
  pure $ lookupHero (toCardCode card) attrs
