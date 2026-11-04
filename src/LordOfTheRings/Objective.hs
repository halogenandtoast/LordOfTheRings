{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Objective (module LordOfTheRings.Objective, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Id
import LordOfTheRings.Json (withDefaults)
import LordOfTheRings.Objective.Cards.CaveTorch
import LordOfTheRings.Objective.Cards.DungeonTorch
import LordOfTheRings.Objective.Cards.GandalfsMap
import LordOfTheRings.Objective.Cards.ShadowKey
import LordOfTheRings.Objective.Cards.SignsOfGollum
import LordOfTheRings.Objective.Cards.GrimbeornTheOld
import LordOfTheRings.Objective.Cards.MistyMountainEagle
import LordOfTheRings.Objective.Cards.Athelas
import LordOfTheRings.Objective.Cards.Wilyador
import LordOfTheRings.Objective.Cards.Scarred
import LordOfTheRings.Objective.Cards.ArnorRavaged
import LordOfTheRings.Objective.Cards.ThaurdirsLegacy
import LordOfTheRings.Objective.Cards.ThaurdirsMajesty
import LordOfTheRings.Objective.Cards.ThaurdirsSpite
import LordOfTheRings.Objective.Cards.GollumTheDeadMarshes
import LordOfTheRings.Objective.Cards.GollumReturnToMirkwood
import LordOfTheRings.Objective.Cards.AbandonedTools
import LordOfTheRings.Objective.Cards.ArwenUndomielObjective
import LordOfTheRings.Objective.Cards.BookOfMazarbul
import LordOfTheRings.Objective.Cards.DurinsGreaves
import LordOfTheRings.Objective.Cards.DurinsAxe
import LordOfTheRings.Objective.Cards.DurinsHelm
import LordOfTheRings.Objective.Cards.Celador
import LordOfTheRings.Objective.Cards.IthilienGuardian
import LordOfTheRings.Objective.Cards.AlcaronsScroll
import LordOfTheRings.Objective.Cards.RescuedVillagers
import LordOfTheRings.Objective.Cards.DeadVillagers
import LordOfTheRings.Objective.Cards.LordAlcaron
import LordOfTheRings.Objective.Cards.ToTheTower
import LordOfTheRings.Objective.Cards.AScrapOfHistory
import LordOfTheRings.Objective.Cards.SecretMap
import LordOfTheRings.Objective.Cards.APrisoner
import LordOfTheRings.Objective.Cards.UnholyAlliance
import LordOfTheRings.Objective.Cards.UpInFlames
import LordOfTheRings.Objective.Cards.PoisonedCounsels
import LordOfTheRings.Objective.Cards.FaramirObjective
import LordOfTheRings.Objective.Cards.LordAlcaronTheBloodOfGondor
import LordOfTheRings.Objective.Cards.MugashObjective
import LordOfTheRings.Objective.Cards.GrimaTheFordsOfIsen
import LordOfTheRings.Objective.Cards.ChiefTurchTheAntleredCrown
import LordOfTheRings.Objective.Cards.NalirTroubleInTharbad
import LordOfTheRings.Objective.Cards.AmarthiulAcrossTheEttenmoors
import LordOfTheRings.Objective.Cards.TheOrcsSearch
import LordOfTheRings.Objective.Cards.TheHavensBurn
import LordOfTheRings.Objective.Cards.CelebrimborsMould
import LordOfTheRings.Objective.Cards.NalirTheNinInEilph
import LordOfTheRings.Objective.Cards.Iarion
import LordOfTheRings.Objective.Cards.CaptiveVillager
import LordOfTheRings.Objective.Cards.KeyOfTheBoar
import LordOfTheRings.Objective.Cards.KeyOfTheWolf
import LordOfTheRings.Objective.Cards.KeyOfTheRaven
import LordOfTheRings.Objective.Cards.HuntingTheOrcs
import LordOfTheRings.Objective.Cards.SavageCounterattack
import LordOfTheRings.Objective.Cards.Daybreak
import LordOfTheRings.Objective.Cards.Amarthiul
import LordOfTheRings.Objective.Cards.AmarthiulTheTreacheryOfRhudaur
import LordOfTheRings.Objective.Cards.DaechanarsBrand
import LordOfTheRings.Objective.Cards.HeirloomOfIarchon
import LordOfTheRings.Objective.Cards.OrdersFromAngmar
import LordOfTheRings.Objective.Cards.Calphon
import LordOfTheRings.Objective.Cards.CaptainSahirObjective
import LordOfTheRings.Objective.Cards.NaasiyahObjective
import LordOfTheRings.Objective.Cards.GateKey
import LordOfTheRings.Objective.Cards.SeaWardTower
import LordOfTheRings.Objective.Cards.TowerOfTheGull
import LordOfTheRings.Objective.Cards.TheBeacon
import LordOfTheRings.Objective.Cards.TowerOfTheHeron
import LordOfTheRings.Objective.Cards.BlackKeyBurden
import LordOfTheRings.Objective.Cards.TheBlackSword
import LordOfTheRings.Objective.Cards.GavinObjective
import LordOfTheRings.Objective.Cards.HorseHairLasso
import LordOfTheRings.Objective.Cards.PoisonedSpear
import LordOfTheRings.Objective.Cards.PitTrap
import LordOfTheRings.Objective.Cards.NooseOfVines
import LordOfTheRings.Objective.Cards.DurinsKey
import LordOfTheRings.Objective.Cards.UrdugsHorn
import LordOfTheRings.Objective.Cards.Aearlang
import LordOfTheRings.Objective.Cards.DocksOfDolAmroth
import LordOfTheRings.Objective.Cards.TheCursedSword
import LordOfTheRings.Objective.Cards.RiseToPower
import LordOfTheRings.Objective.Cards.Assassination
import LordOfTheRings.Objective.Cards.TheCabalsChampion
import LordOfTheRings.Objective.Cards.OnTrack
import LordOfTheRings.Objective.Cards.Wormsbane
import LordOfTheRings.Objective.Cards.SeizeThem
import LordOfTheRings.Objective.Cards.BannerOfTheSerpent
import LordOfTheRings.Objective.Cards.TheSerpentsGarb
import LordOfTheRings.Objective.Cards.SummonsFromMordor
import LordOfTheRings.Objective.Cards.TheCaptivesOfCirithGurat
import LordOfTheRings.Objective.Cards.Abaan
import LordOfTheRings.Objective.Cards.TheGoblinsAssault
import LordOfTheRings.Objective.Cards.TheHeroesDefense
import LordOfTheRings.Objective.Cards.CryptKey
import LordOfTheRings.Objective.Cards.OldLantern
import LordOfTheRings.Objective.Cards.ScathasTeeth
import LordOfTheRings.Objective.Cards.DragonSign
import LordOfTheRings.Objective.Cards.SearchingForAWayOut
import LordOfTheRings.Objective.Cards.UnderTheWatchfulEye
import LordOfTheRings.Objective.Cards.TheArmiesOfMordor
import LordOfTheRings.Objective.Cards.CoveredInDarkness
import LordOfTheRings.Objective.Cards.EasterlingPursuit
import LordOfTheRings.Objective.Cards.Bribes
import LordOfTheRings.Objective.Cards.SubmitOrDie
import LordOfTheRings.Objective.Cards.SauronWorship
import LordOfTheRings.Objective.Cards.EvidenceOfTheCult
import LordOfTheRings.Objective.Cards.TheWainriders
import LordOfTheRings.Objective.Cards.TheChallengers
import LordOfTheRings.Objective.Cards.OvercomeByFear (overcomeByFear)
import LordOfTheRings.Objective.Cards.TheOneRingSaga
import LordOfTheRings.Objective.Cards.TrollPurse
import LordOfTheRings.Objective.Cards.TrollKey
import LordOfTheRings.Objective.Cards.ASmellySack
import LordOfTheRings.Objective.Cards.AStrongSack
import LordOfTheRings.Objective.Cards.AToughSack
import LordOfTheRings.Objective.Cards.ALargeSack
import LordOfTheRings.Objective.Cards.ADeepSack
import LordOfTheRings.Objective.Cards.AFoulSack
import LordOfTheRings.Objective.Cards.AWornSack
import LordOfTheRings.Objective.Cards.BilbosMagicRing
import LordOfTheRings.Objective.Cards.ABarePatch (aBarePatch)
import LordOfTheRings.Objective.Cards.GandalfsDelay
import LordOfTheRings.Objective.Cards.GildorInglorionObjective
import LordOfTheRings.Objective.Cards.AnEvilWound
import LordOfTheRings.Objective.Cards.MinasTirith (minasTirith)
import LordOfTheRings.Objective.Cards.TheCorsairFleetGondor (theCorsairFleetGondor)
import LordOfTheRings.Objective.Cards.TheCorsairFleet (theCorsairFleet)
import LordOfTheRings.Objective.Runner ()
import LordOfTheRings.Objective.Types as X
import LordOfTheRings.Prelude

-- | A stored objective is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Objective where
  parseJSON v = do
    attrs <- parseJSON @ObjectiveAttrs (withDefaults [("guard", Null)] v)
    pure $ overAttrs (const attrs) $ lookupObjective (objectiveCardCode attrs) (objectiveCardId attrs) (objectiveId attrs)

instance RunMessage Objective where
  runMessage msg (Objective a) = Objective <$> runMessage msg a

createObjective :: EncounterCard -> ObjectiveId -> Objective
createObjective card = lookupObjective (toCardCode card) (ecId card)

lookupObjective :: (HasCallStack) => CardCode -> CardId -> ObjectiveId -> Objective
lookupObjective cardCode cardId ident = case Map.lookup cardCode allObjectives of
  Just (SomeObjectiveCard builder) -> Objective $ cbCardBuilder builder cardId ident
  Nothing -> error $ "Unknown objective: " <> show cardCode

newObjective :: (MonadRandom m) => EncounterCard -> m Objective
newObjective card = createObjective card <$> getRandom

allObjectives :: Map CardCode SomeObjectiveCard
allObjectives =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeObjectiveCard builder) <- [SomeObjectiveCard gandalfsMap, SomeObjectiveCard dungeonTorch, SomeObjectiveCard caveTorch, SomeObjectiveCard shadowKey, SomeObjectiveCard signsOfGollum, SomeObjectiveCard grimbeornTheOld, SomeObjectiveCard athelas, SomeObjectiveCard wilyador, SomeObjectiveCard scarred, SomeObjectiveCard gollumTheDeadMarshes, SomeObjectiveCard gollumReturnToMirkwood, SomeObjectiveCard abandonedTools, SomeObjectiveCard bookOfMazarbul, SomeObjectiveCard arwenUndomielTheRedhornGate, SomeObjectiveCard arwenUndomielRoadToRivendell, SomeObjectiveCard durinsGreaves, SomeObjectiveCard durinsAxe, SomeObjectiveCard durinsHelm, SomeObjectiveCard celador, SomeObjectiveCard ithilienGuardian, SomeObjectiveCard alcaronsScroll, SomeObjectiveCard rescuedVillagers, SomeObjectiveCard deadVillagers, SomeObjectiveCard lordAlcaron, SomeObjectiveCard toTheTower, SomeObjectiveCard aScrapOfHistory, SomeObjectiveCard secretMap, SomeObjectiveCard aPrisoner, SomeObjectiveCard unholyAlliance, SomeObjectiveCard upInFlames, SomeObjectiveCard poisonedCounsels, SomeObjectiveCard faramirObjective, SomeObjectiveCard lordAlcaronTheBloodOfGondor, SomeObjectiveCard mugashObjective, SomeObjectiveCard grimaTheFordsOfIsen, SomeObjectiveCard chiefTurchTheAntleredCrown, SomeObjectiveCard nalirTroubleInTharbad, SomeObjectiveCard theOrcsSearch, SomeObjectiveCard celebrimborsMould, SomeObjectiveCard nalirTheNinInEilph, SomeObjectiveCard keyOfTheBoar, SomeObjectiveCard keyOfTheWolf, SomeObjectiveCard keyOfTheRaven, SomeObjectiveCard iarion, SomeObjectiveCard huntingTheOrcs, SomeObjectiveCard savageCounterattack, SomeObjectiveCard daybreak, SomeObjectiveCard nightfall, SomeObjectiveCard amarthiul, SomeObjectiveCard amarthiulAcrossTheEttenmoors, SomeObjectiveCard amarthiulTheTreacheryOfRhudaur, SomeObjectiveCard daechanarsBrand, SomeObjectiveCard heirloomOfIarchon, SomeObjectiveCard ordersFromAngmar, SomeObjectiveCard calphon, SomeObjectiveCard captainSahirObjective, SomeObjectiveCard naasiyahObjective, SomeObjectiveCard theHavensBurn, SomeObjectiveCard gateKey, SomeObjectiveCard gateKeyUpgraded, SomeObjectiveCard seaWardTower, SomeObjectiveCard towerOfTheGull, SomeObjectiveCard theBeacon, SomeObjectiveCard towerOfTheHeron, SomeObjectiveCard captiveVillager, SomeObjectiveCard aearlang, SomeObjectiveCard docksOfDolAmroth, SomeObjectiveCard theCursedSword, SomeObjectiveCard riseToPower, SomeObjectiveCard assassination, SomeObjectiveCard theCabalsChampion, SomeObjectiveCard arnorRavaged, SomeObjectiveCard thaurdirsLegacy, SomeObjectiveCard thaurdirsMajesty, SomeObjectiveCard thaurdirsSpite, SomeObjectiveCard blackKeyBurden, SomeObjectiveCard blackKeyBurdenUpgraded, SomeObjectiveCard theBlackSword, SomeObjectiveCard gavinObjective, SomeObjectiveCard offTrack, SomeObjectiveCard onTrack, SomeObjectiveCard seizeThem, SomeObjectiveCard bannerOfTheSerpent, SomeObjectiveCard theSerpentsGarb, SomeObjectiveCard summonsFromMordor, SomeObjectiveCard theCaptivesOfCirithGurat, SomeObjectiveCard abaan, SomeObjectiveCard horseHairLasso, SomeObjectiveCard poisonedSpear, SomeObjectiveCard pitTrap, SomeObjectiveCard nooseOfVines, SomeObjectiveCard theGoblinsAssault, SomeObjectiveCard theHeroesDefense, SomeObjectiveCard cryptKey, SomeObjectiveCard oldLantern, SomeObjectiveCard scathasTeeth, SomeObjectiveCard wormsbane, SomeObjectiveCard wormsbaneReforged, SomeObjectiveCard dragonSign, SomeObjectiveCard durinsKey, SomeObjectiveCard urdugsHorn, SomeObjectiveCard searchingForAWayOut, SomeObjectiveCard underTheWatchfulEye, SomeObjectiveCard theArmiesOfMordor, SomeObjectiveCard coveredInDarkness, SomeObjectiveCard easterlingPursuit, SomeObjectiveCard bribes, SomeObjectiveCard submitOrDie, SomeObjectiveCard sauronWorship, SomeObjectiveCard evidenceOfTheCult, SomeObjectiveCard theWainriders, SomeObjectiveCard theChallengers, SomeObjectiveCard theOneRingTheLandOfShadow, SomeObjectiveCard theOneRingTheMountainOfFire, SomeObjectiveCard overcomeByFear, SomeObjectiveCard theOneRingTheRoadDarkens, SomeObjectiveCard theOneRingTheBlackRiders, SomeObjectiveCard mistyMountainEagle, SomeObjectiveCard trollPurse, SomeObjectiveCard trollKey, SomeObjectiveCard aSmellySack, SomeObjectiveCard aStrongSack, SomeObjectiveCard aToughSack, SomeObjectiveCard aLargeSack, SomeObjectiveCard aDeepSack, SomeObjectiveCard aFoulSack, SomeObjectiveCard aWornSack, SomeObjectiveCard bilbosMagicRing, SomeObjectiveCard aBarePatch, SomeObjectiveCard gandalfsDelay, SomeObjectiveCard gildorInglorionObjective, SomeObjectiveCard anEvilWound, SomeObjectiveCard theCorsairFleet, SomeObjectiveCard minasTirith, SomeObjectiveCard theCorsairFleetGondor]
    ]
