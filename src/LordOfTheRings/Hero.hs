{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Hero (module LordOfTheRings.Hero.Type, module LordOfTheRings.Hero) where

import Control.Monad.Random (MonadRandom (getRandom))
import Data.Aeson (ToJSON)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Records
import LordOfTheRings.Card
import LordOfTheRings.Hero.Type
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Run

instance Run Hero where
  run msg (Hero a) = Hero <$> run msg a

instance Run HeroAttrs where
  run msg attrs = case msg of
    BeginPhase ResourcePhase -> do
      pure $ attrs {resources = attrs.resources + 1}
    SpendHeroResources hId n | hId == attrs.id -> do
      pure $ attrs {resources = max 0 (attrs.resources - n)}
    _ -> pure attrs

instance HasField "threat" HeroAttrs Int where
  getField a = a.card.def.details.threat

instance HasField "id" Hero HeroId where
  getField (Hero a) = (heroAttrs a).id

instance HasField "threat" Hero Int where
  getField (Hero a) = (heroAttrs a).threat

instance HasField "sphere" Hero Sphere where
  getField (Hero a) = (heroAttrs a).sphere

instance HasField "resources" Hero Int where
  getField (Hero a) = (heroAttrs a).resources

newtype Beravor = Beravor HeroAttrs
  deriving newtype (Show, ToJSON)
instance IsHero Beravor
instance Run Beravor where
  run msg (Beravor a) = Beravor <$> run msg a

newtype Eowyn = Eowyn HeroAttrs
  deriving newtype (Show, ToJSON)
instance IsHero Eowyn
instance Run Eowyn where
  run msg (Eowyn a) = Eowyn <$> run msg a

newtype Theodred = Theodred HeroAttrs
  deriving newtype (Show, ToJSON)
instance IsHero Theodred
instance Run Theodred where
  run msg (Theodred a) = Theodred <$> run msg a

lookupHero :: CardCode -> HeroAttrs -> Hero
lookupHero cCode = fromMaybe (error "No such hero") $ Map.lookup cCode allData
 where
  allData = Map.fromList [("01012", Hero . Beravor), ("01007", Hero . Eowyn), ("01002", Hero . Theodred)]

newHero :: (MonadRandom m) => Card 'HeroKind -> m Hero
newHero card = do
  heroId <- getRandom
  let attrs = HeroAttrs {id = heroId, card, resources = 0, sphere = card.def.sphere}
  pure $ lookupHero card.def.code attrs

allHeroes :: Map CardCode (CardDef 'HeroKind)
allHeroes = Map.fromList [("01012", beravor), ("01007", eowyn), ("01002", theodred)]

hero :: CardCode -> Name -> Sphere -> HeroDetails -> CardDef 'HeroKind
hero code name sphere details = CardDef {code, name, unique = True, kind = SHeroKind, details, sphere}

beravor :: CardDef 'HeroKind
beravor = hero "01012" "Beravor" Lore (HeroDetails 10 2 2 2 4)

eowyn :: CardDef 'HeroKind
eowyn = hero "01007" "Eowyn" Spirit (HeroDetails 9 4 1 1 3)

theodred :: CardDef 'HeroKind
theodred = hero "01002" "Theodred" Leadership (HeroDetails 8 1 2 1 4)
