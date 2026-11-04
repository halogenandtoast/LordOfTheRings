{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Hero (module LordOfTheRings.Hero, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ability.Types (Ability (..))
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasAbilities (getAbilities)
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Classes.HasQueue (captureWith, pushAll)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Message (Message (DoStep, UseCardAbility))
import LordOfTheRings.Modifier (ModifierType (GainGameTextOf))
import LordOfTheRings.Source (Source (HeroSource))
import LordOfTheRings.Target (Target (HeroTarget))
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
import LordOfTheRings.Hero.Cards.SarumanLore
import LordOfTheRings.Hero.Cards.GildorInglorionLeadership
import LordOfTheRings.Hero.Cards.FrodoBagginsLeadership
import LordOfTheRings.Hero.Cards.ThorinStonehelm
import LordOfTheRings.Hero.Cards.GwaihirTheLandOfSorrow
import LordOfTheRings.Hero.Cards.AragornSpirit
import LordOfTheRings.Hero.Runner ()
import LordOfTheRings.Hero.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | A stored hero is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Hero where
  parseJSON v = do
    attrs <- parseJSON @HeroAttrs v
    pure $ overAttrs (const attrs) $ lookupHero (heroCardCode attrs) (heroCardId attrs) (heroId attrs) (heroOwner attrs)

{- | A hero gains the game text of a hero card through 'GainGameTextOf' (Thorongil). The
abilities it gains are its own, numbered past 'gainedTextIndex' (see 'withGainedAbilities');
using one, that card, built as the hero, resolves it, and the later steps it pushes for it
come back numbered so again.
-}
instance RunMessage Hero where
  runMessage msg h@(Hero a) = do
    gained <- gainedTextOf h
    let mine = HeroSource (toId h)
        viaGained code m = do
          (card, msgs) <- lift $ captureWith $ runMessage m (asGainedText code h)
          pushAll $ map (renumber mine) msgs
          pure $ overAttrs (\x -> (toAttrs card) {heroGainedAbilities = heroGainedAbilities x}) h
    case (gained, msg) of
      (code : _, UseCardAbility pid s n ws)
        | s == mine, n > gainedTextIndex -> viaGained code (UseCardAbility pid s (n - gainedTextIndex) ws)
      (code : _, DoStep k (UseCardAbility pid s n ws))
        | s == mine, n > gainedTextIndex -> viaGained code (DoStep k (UseCardAbility pid s (n - gainedTextIndex) ws))
      _ -> Hero <$> runMessage msg a
   where
    renumber mine = \case
      DoStep k (UseCardAbility pid s n ws) | s == mine, n <= gainedTextIndex -> DoStep k (UseCardAbility pid s (gainedTextIndex + n) ws)
      m -> m

-- | The hero's gained abilities, as the hero cards whose game text it gains give them,
-- numbered past 'gainedTextIndex'.
withGainedAbilities :: [CardCode] -> Hero -> Hero
withGainedAbilities codes h =
  overAttrs
    (\x -> x {heroGainedAbilities = [ab {abilityIndex = gainedTextIndex + ab.abilityIndex} | code <- codes, ab <- getAbilities (asGainedText code h)]})
    h

-- | The abilities of a hero card whose text a hero gains are numbered past this.
gainedTextIndex :: Int
gainedTextIndex = 100

-- | The codes of the hero cards whose game text the hero gains.
gainedTextOf :: HasGame m => Hero -> m [CardCode]
gainedTextOf h = mapMaybe (\case GainGameTextOf code -> Just code; _ -> Nothing) <$> getModifiers (HeroTarget $ toId h)

-- | The hero card, built as the hero: its text, the hero's state, but none of the text the
-- hero gains.
asGainedText :: CardCode -> Hero -> Hero
asGainedText code h = overAttrs (const attrs) $ lookupHero code attrs.heroCardId attrs.heroId attrs.heroOwner
 where
  attrs = (toAttrs h) {heroGainedAbilities = []}

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
        , SomeHeroCard sarumanLore
        , SomeHeroCard gildorInglorionLeadership
        , SomeHeroCard frodoBagginsLeadership
        , SomeHeroCard thorinStonehelm
        , SomeHeroCard gwaihirTheLandOfSorrow
        , SomeHeroCard aragornSpirit
        ]
    ]
