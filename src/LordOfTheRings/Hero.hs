{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Hero (module LordOfTheRings.Hero.Type, module LordOfTheRings.Hero) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Card.PlayerCard qualified as PC
import LordOfTheRings.Hero.Type
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Name
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Run

instance Run Hero where
  run msg (Hero a) = Hero <$> run msg a

instance Run HeroAttrs where
  run msg attrs = case msg of
    BeginPhase ResourcePhase -> do
      push $ GainHeroResources attrs.id 1
      pure attrs
    GainHeroResources hId n | hId == attrs.id -> do
      pure $ attrs {resources = attrs.resources + n}
    SpendHeroResources hId n | hId == attrs.id -> do
      pure $ attrs {resources = max 0 (attrs.resources - n)}
    _ -> pure attrs

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

newHero :: (MonadRandom m) => PlayerId -> PlayerCard 'HeroKind -> m Hero
newHero player card = do
  heroId <- getRandom
  let attrs = HeroAttrs {id = heroId, card, player, resources = 0, sphere = card.def.sphere}
  pure $ lookupHero card.def.code attrs

type HeroCard = PlayerCardDef 'HeroKind

allHeroes :: Map CardCode HeroCard
allHeroes = Map.fromList [("01012", beravor), ("01007", eowyn), ("01002", theodred)]

hero :: CardCode -> Name -> Sphere -> HeroDetails -> HeroCard
hero code name sphere details = PlayerCardDef {code, name, PC.unique = True, kind = SHeroKind, details, sphere}

beravor :: HeroCard
beravor = hero "01012" "Beravor" Lore (HeroDetails 10 2 2 2 4)

eowyn :: HeroCard
eowyn = hero "01007" "Eowyn" Spirit (HeroDetails 9 4 1 1 3)

theodred :: HeroCard
theodred = hero "01002" "Theodred" Leadership (HeroDetails 8 1 2 1 4)
