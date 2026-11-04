{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Hero (module LordOfTheRings.Hero, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Hero.Cards.Eomer
import LordOfTheRings.Hero.Cards.Hirgon
import LordOfTheRings.Hero.Cards.Fastred
import LordOfTheRings.Hero.Cards.Lothiriel
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
import LordOfTheRings.Hero.Cards.BilboBaggins
import LordOfTheRings.Hero.Cards.FrodoBaggins
import LordOfTheRings.Hero.Cards.BrandSonOfBain
import LordOfTheRings.Hero.Cards.BoromirTactics
import LordOfTheRings.Hero.Cards.Celeborn
import LordOfTheRings.Hero.Cards.Galadriel
import LordOfTheRings.Hero.Cards.HaldirOfLorien
import LordOfTheRings.Hero.Cards.Elrond
import LordOfTheRings.Hero.Cards.DainIronfoot
import LordOfTheRings.Hero.Cards.Bifur
import LordOfTheRings.Hero.Cards.Nori
import LordOfTheRings.Hero.Cards.Ori
import LordOfTheRings.Hero.Cards.PrinceImrahil
import LordOfTheRings.Hero.Cards.Beregond
import LordOfTheRings.Hero.Cards.Boromir
import LordOfTheRings.Hero.Cards.Faramir
import LordOfTheRings.Hero.Cards.Mablung
import LordOfTheRings.Hero.Cards.Dwalin
import LordOfTheRings.Hero.Cards.Elrohir
import LordOfTheRings.Hero.Cards.Elladan
import LordOfTheRings.Hero.Cards.AragornLore
import LordOfTheRings.Hero.Cards.Hama
import LordOfTheRings.Hero.Cards.GlorfindelSpirit
import LordOfTheRings.Hero.Cards.ArwenUndomiel
import LordOfTheRings.Hero.Cards.EowynTactics
import LordOfTheRings.Hero.Cards.HirluinTheFair
import LordOfTheRings.Hero.Cards.Mirlonde
import LordOfTheRings.Hero.Cards.Pippin
import LordOfTheRings.Hero.Cards.Caldara
import LordOfTheRings.Hero.Cards.Theoden
import LordOfTheRings.Hero.Cards.Grima
import LordOfTheRings.Hero.Cards.Idraen
import LordOfTheRings.Hero.Cards.Erkenbrand
import LordOfTheRings.Hero.Cards.Amarthiul
import LordOfTheRings.Hero.Cards.Rossiel
import LordOfTheRings.Hero.Cards.Dori
import LordOfTheRings.Hero.Cards.Erestor
import LordOfTheRings.Hero.Cards.AragornTactics
import LordOfTheRings.Hero.Cards.Halbarad
import LordOfTheRings.Hero.Cards.MerrySpirit
import LordOfTheRings.Hero.Cards.CirdanTheShipwright
import LordOfTheRings.Hero.Cards.GaldorOfTheHavensHero
import LordOfTheRings.Hero.Cards.DenethorLeadership
import LordOfTheRings.Hero.Cards.Lanwyn
import LordOfTheRings.Hero.Cards.ElfhelmLeadership
import LordOfTheRings.Hero.Cards.Argalad
import LordOfTheRings.Hero.Cards.Naasiyah
import LordOfTheRings.Hero.Cards.PrinceImrahilTactics
import LordOfTheRings.Hero.Cards.KahlielObjective
import LordOfTheRings.Hero.Cards.Firyal
import LordOfTheRings.Hero.Cards.Yazan
import LordOfTheRings.Hero.Cards.Jubayr
import LordOfTheRings.Hero.Cards.GimliLeadership
import LordOfTheRings.Hero.Cards.LegolasSpirit
import LordOfTheRings.Hero.Cards.Kahliel
import LordOfTheRings.Hero.Cards.Thurindir
import LordOfTheRings.Hero.Cards.FolcoBoffin
import LordOfTheRings.Hero.Cards.Quickbeam
import LordOfTheRings.Hero.Cards.BrandSonOfBainLeadership
import LordOfTheRings.Hero.Cards.BardSonOfBrand
import LordOfTheRings.Hero.Cards.GrimbeornTheOld
import LordOfTheRings.Hero.Cards.Haldan
import LordOfTheRings.Hero.Cards.Thranduil
import LordOfTheRings.Hero.Cards.DainIronfootSpirit
import LordOfTheRings.Hero.Cards.Smeagol
import LordOfTheRings.Hero.AllyHero (allyHero)
import LordOfTheRings.Hero.Cards.BilboBagginsTactics
import LordOfTheRings.Hero.Cards.Radagast
import LordOfTheRings.Hero.Cards.FrodoBagginsLeadership
import LordOfTheRings.Hero.Cards.ThorinStonehelm
import LordOfTheRings.Hero.Runner ()
import LordOfTheRings.Hero.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | A stored hero is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Hero where
  parseJSON v = do
    attrs <- parseJSON @HeroAttrs v
    pure $ overAttrs (const attrs) $ lookupHero (heroCardCode attrs) (heroCardId attrs) (heroId attrs) (heroOwner attrs)

instance RunMessage Hero where
  runMessage msg (Hero a) = Hero <$> runMessage msg a

createHero :: PlayerCard -> HeroId -> PlayerId -> Hero
createHero card hid pid = lookupHero (toCardCode card) (pcId card) hid pid

lookupHero :: (HasCallStack) => CardCode -> CardId -> HeroId -> PlayerId -> Hero
lookupHero cardCode cardId hid pid = case Map.lookup cardCode allHeroes of
  Just (SomeHeroCard builder) -> Hero $ cbCardBuilder builder cardId (hid, pid)
  Nothing
    | cdCardType (toCardDef cardCode) == AllyType -> Hero $ cbCardBuilder (allyHero cardCode) cardId (hid, pid)
    | otherwise -> error $ "Unknown hero: " <> show cardCode

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
        , SomeHeroCard bilboBaggins
        , SomeHeroCard frodoBaggins
        , SomeHeroCard brandSonOfBain
        , SomeHeroCard boromirTactics
        , SomeHeroCard eomer
        , SomeHeroCard hirgon
        , SomeHeroCard fastred
        , SomeHeroCard lothiriel
        , SomeHeroCard celeborn
        , SomeHeroCard galadriel
        , SomeHeroCard haldirOfLorien
        , SomeHeroCard elrond
        , SomeHeroCard dainIronfoot
        , SomeHeroCard bifur
        , SomeHeroCard nori
        , SomeHeroCard ori
        , SomeHeroCard princeImrahil
        , SomeHeroCard beregond
        , SomeHeroCard boromir
        , SomeHeroCard faramir
        , SomeHeroCard mablung
        , SomeHeroCard dwalin
        , SomeHeroCard elrohir
        , SomeHeroCard elladan
        , SomeHeroCard aragornLore
        , SomeHeroCard hama
        , SomeHeroCard glorfindelSpirit
        , SomeHeroCard arwenUndomiel
        , SomeHeroCard eowynTactics
        , SomeHeroCard hirluinTheFair
        , SomeHeroCard mirlonde
        , SomeHeroCard pippin
        , SomeHeroCard caldara
        , SomeHeroCard theoden
        , SomeHeroCard grima
        , SomeHeroCard idraen
        , SomeHeroCard erkenbrand
        , SomeHeroCard amarthiul
        , SomeHeroCard rossiel
        , SomeHeroCard dori
        , SomeHeroCard erestor
        , SomeHeroCard aragornTactics
        , SomeHeroCard halbarad
        , SomeHeroCard merrySpirit
        , SomeHeroCard cirdanTheShipwright
        , SomeHeroCard galdorOfTheHavensHero
        , SomeHeroCard denethorLeadership
        , SomeHeroCard lanwyn
        , SomeHeroCard elfhelmLeadership
        , SomeHeroCard argalad
        , SomeHeroCard naasiyah
        , SomeHeroCard princeImrahilTactics
        , SomeHeroCard kahlielObjective
        , SomeHeroCard firyal
        , SomeHeroCard yazan
        , SomeHeroCard jubayr
        , SomeHeroCard gimliLeadership
        , SomeHeroCard legolasSpirit
        , SomeHeroCard kahliel
        , SomeHeroCard thurindir
        , SomeHeroCard folcoBoffin
        , SomeHeroCard quickbeam
        , SomeHeroCard bilboBagginsTactics
        , SomeHeroCard radagast
        , SomeHeroCard brandSonOfBainLeadership
        , SomeHeroCard bardSonOfBrand
        , SomeHeroCard grimbeornTheOld
        , SomeHeroCard haldan
        , SomeHeroCard thranduil
        , SomeHeroCard dainIronfootSpirit
        , SomeHeroCard smeagol
        , SomeHeroCard frodoBagginsLeadership
        , SomeHeroCard thorinStonehelm
        ]
    ]
