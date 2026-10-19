{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Hero (module LordOfTheRings.Hero, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Hero.Cards.Aragorn
import LordOfTheRings.Hero.Cards.Beravor
import LordOfTheRings.Hero.Cards.Denethor
import LordOfTheRings.Hero.Cards.Dunhere
import LordOfTheRings.Hero.Cards.Eleanor
import LordOfTheRings.Hero.Cards.Eowyn
import LordOfTheRings.Hero.Cards.Gimli
import LordOfTheRings.Hero.Cards.Gloin
import LordOfTheRings.Hero.Cards.Glorfindel
import LordOfTheRings.Hero.Cards.Legolas
import LordOfTheRings.Hero.Cards.Thalin
import LordOfTheRings.Hero.Cards.Theodred
import LordOfTheRings.Hero.Runner ()
import LordOfTheRings.Hero.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

instance RunMessage Hero where
  runMessage msg (Hero a) = Hero <$> runMessage msg a

createHero :: PlayerCard -> HeroId -> PlayerId -> Hero
createHero card hid pid = lookupHero (toCardCode card) (pcId card) hid pid

lookupHero :: (HasCallStack) => CardCode -> CardId -> HeroId -> PlayerId -> Hero
lookupHero cardCode cardId hid pid = case Map.lookup cardCode allHeroes of
  Just (SomeHeroCard builder) -> Hero $ cbCardBuilder builder cardId (hid, pid)
  Nothing -> error $ "Unknown hero: " <> show cardCode

newHero :: (MonadRandom m) => PlayerId -> PlayerCard -> m Hero
newHero pid card = do
  hid <- getRandom
  pure $ createHero card hid pid

allHeroes :: Map CardCode SomeHeroCard
allHeroes =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeHeroCard builder) <-
        [ SomeHeroCard aragorn
        , SomeHeroCard theodred
        , SomeHeroCard gloin
        , SomeHeroCard gimli
        , SomeHeroCard legolas
        , SomeHeroCard thalin
        , SomeHeroCard eowyn
        , SomeHeroCard eleanor
        , SomeHeroCard dunhere
        , SomeHeroCard denethor
        , SomeHeroCard glorfindel
        , SomeHeroCard beravor
        ]
    ]
