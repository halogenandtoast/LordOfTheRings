{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Ally (module LordOfTheRings.Ally, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Ally.Cards.Beorn
import LordOfTheRings.Ally.Cards.BrokIronfist
import LordOfTheRings.Ally.Cards.DaughterOfTheNimrodel
import LordOfTheRings.Ally.Cards.EreborHammersmith
import LordOfTheRings.Ally.Cards.Faramir
import LordOfTheRings.Ally.Cards.Gandalf
import LordOfTheRings.Ally.Cards.Gleowine
import LordOfTheRings.Ally.Cards.GondorianSpearman
import LordOfTheRings.Ally.Cards.GuardOfTheCitadel
import LordOfTheRings.Ally.Cards.HenamarthRiversong
import LordOfTheRings.Ally.Cards.HorsebackArcher
import LordOfTheRings.Ally.Cards.LongbeardOrcSlayer
import LordOfTheRings.Ally.Cards.LorienGuide
import LordOfTheRings.Ally.Cards.MinerOfTheIronHills
import LordOfTheRings.Ally.Cards.NorthernTracker
import LordOfTheRings.Ally.Cards.SilverlodeArcher
import LordOfTheRings.Ally.Cards.SnowbournScout
import LordOfTheRings.Ally.Cards.SonOfArnor
import LordOfTheRings.Ally.Cards.VeteranAxehand
import LordOfTheRings.Ally.Cards.WanderingTook
import LordOfTheRings.Ally.Cards.WingedGuardian
import LordOfTheRings.Ally.Cards.RivendellMinstrel
import LordOfTheRings.Ally.Cards.BeorningBeekeeper
import LordOfTheRings.Ally.Cards.LongbeardMapMaker
import LordOfTheRings.Ally.Cards.GrimbeornTheOld
import LordOfTheRings.Ally.Cards.Landroval
import LordOfTheRings.Ally.Cards.HaldirOfLorien
import LordOfTheRings.Ally.Cards.Radagast
import LordOfTheRings.Ally.Cards.Wilyador
import LordOfTheRings.Ally.Runner ()
import LordOfTheRings.Ally.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | A stored ally is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Ally where
  parseJSON v = do
    attrs <- parseJSON @AllyAttrs v
    pure $ overAttrs (const attrs) $ lookupAlly (allyCardCode attrs) (allyCardId attrs) (allyId attrs) (allyOwner attrs)

instance RunMessage Ally where
  runMessage msg (Ally a) = Ally <$> runMessage msg a

createAlly :: PlayerCard -> AllyId -> PlayerId -> Ally
createAlly card hid pid = lookupAlly (toCardCode card) (pcId card) hid pid

lookupAlly :: (HasCallStack) => CardCode -> CardId -> AllyId -> PlayerId -> Ally
lookupAlly cardCode cardId hid pid = case Map.lookup cardCode allAllies of
  Just (SomeAllyCard builder) -> Ally $ cbCardBuilder builder cardId (hid, pid)
  Nothing -> error $ "Unknown ally: " <> show cardCode

newAlly :: (MonadRandom m) => PlayerId -> PlayerCard -> m Ally
newAlly pid card = do
  hid <- getRandom
  pure $ createAlly card hid pid

allAllies :: Map CardCode SomeAllyCard
allAllies =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeAllyCard builder) <-
        [ SomeAllyCard guardOfTheCitadel
        , SomeAllyCard faramir
        , SomeAllyCard sonOfArnor
        , SomeAllyCard snowbournScout
        , SomeAllyCard silverlodeArcher
        , SomeAllyCard longbeardOrcSlayer
        , SomeAllyCard brokIronfist
        , SomeAllyCard veteranAxehand
        , SomeAllyCard gondorianSpearman
        , SomeAllyCard horsebackArcher
        , SomeAllyCard beorn
        , SomeAllyCard wanderingTook
        , SomeAllyCard lorienGuide
        , SomeAllyCard northernTracker
        , SomeAllyCard daughterOfTheNimrodel
        , SomeAllyCard ereborHammersmith
        , SomeAllyCard henamarthRiversong
        , SomeAllyCard minerOfTheIronHills
        , SomeAllyCard gleowine
        , SomeAllyCard gandalf
        , SomeAllyCard wingedGuardian
        , SomeAllyCard rivendellMinstrel
        , SomeAllyCard beorningBeekeeper
        , SomeAllyCard longbeardMapMaker
        , SomeAllyCard grimbeornTheOld
        , SomeAllyCard landroval
        , SomeAllyCard haldirOfLorien
        , SomeAllyCard radagast
        , SomeAllyCard wilyador
        ]
    ]
