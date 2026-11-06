module LordOfTheRings.Campaign.CardDefs (module LordOfTheRings.Campaign.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.CardType
import LordOfTheRings.Card.Code
import LordOfTheRings.Name (Name)
import LordOfTheRings.Prelude

{- | The campaign cards: each sits next to the quest deck in campaign mode, its setup
followed as the scenario is set up and the resolution on its back once it is won. What
they say is their scenario's to do; see "LordOfTheRings.Campaign".
-}
allCampaignCards :: Map CardCode CardDef
allCampaignCards =
  Map.fromList
    $ map
      (\def -> (cdCardCode def, def))
      [ passageThroughMirkwoodCampaign
      , theOathCampaign
      , theCavesOfNibinDumCampaign
      , journeyAlongTheAnduinCampaign
      , escapeFromDolGuldurCampaign
      , voyageAcrossBelegaerCampaign
      , theFateOfNumenorCampaign
      , raidOnTheGreyHavensCampaign
      , flightOfTheStormcallerCampaign
      , theThingInTheDepthsCampaign
      , templeOfTheDeceivedCampaign
      , theDrownedRuinsCampaign
      , aStormOnCobasHavenCampaign
      , theCityOfCorsairsCampaign
      , intrudersInChetwoodCampaign
      , theWeatherHillsCampaign
      , deadmensDikeCampaign
      , theWastesOfEriadorCampaign
      , escapeFromMountGramCampaign
      , acrossTheEttenmoorsCampaign
      , theTreacheryOfRhudaurCampaign
      , theBattleOfCarnDumCampaign
      , theDreadRealmCampaign
      , aShadowOfThePastCampaign
      , aKnifeInTheDarkCampaign
      , flightToTheFordCampaign
      , theRingGoesSouthCampaign
      , journeyInTheDarkCampaign
      , breakingOfTheFellowshipCampaign
      , theUrukHaiCampaign
      , helmsDeepCampaign
      , theRoadToIsengardCampaign
      , thePassageOfTheMarshesCampaign
      , journeyToTheCrossRoadsCampaign
      , shelobsLairCampaign
      , thePassingOfTheGreyCompanyCampaign
      , theSiegeOfGondorCampaign
      , theBattleOfThePelennorFieldsCampaign
      , theTowerOfCirithUngolCampaign
      , theBlackGateOpensCampaign
      , mountDoomCampaign
      , journeyUpTheAnduinCampaign
      , lostInMirkwoodCampaign
      , theKingsQuestCampaign
      , theWitheredHeathCampaign
      , roamAcrossRhovanionCampaign
      , fireInTheNightCampaign
      , theGhostOfFramsburgCampaign
      , mountGundabadCampaign
      , theFateOfWilderlandCampaign
      ]

-- | Double-sided, in no encounter set, so no encounter deck gathers it.
campaignCard :: CardCode -> Name -> CardDef
campaignCard cCode name = (emptyCardDef cCode name CampaignType) {cdDoubleSided = True}

passageThroughMirkwoodCampaign :: CardDef
passageThroughMirkwoodCampaign = campaignCard "RC001" "Passage Through Mirkwood"

journeyAlongTheAnduinCampaign :: CardDef
journeyAlongTheAnduinCampaign = campaignCard "RC006" "Journey Along the Anduin"

escapeFromDolGuldurCampaign :: CardDef
escapeFromDolGuldurCampaign = campaignCard "RC009" "Escape from Dol Guldur"

-- * The Dream-chaser Campaign Expansion

voyageAcrossBelegaerCampaign :: CardDef
voyageAcrossBelegaerCampaign = campaignCard "DCC171" "Voyage Across Belegaer"

theFateOfNumenorCampaign :: CardDef
theFateOfNumenorCampaign = campaignCard "DCC179" "The Fate of Númenor"

raidOnTheGreyHavensCampaign :: CardDef
raidOnTheGreyHavensCampaign = campaignCard "DCC182" "Raid on the Grey Havens"

flightOfTheStormcallerCampaign :: CardDef
flightOfTheStormcallerCampaign = campaignCard "DCC191" "Flight of the Stormcaller"

theThingInTheDepthsCampaign :: CardDef
theThingInTheDepthsCampaign = campaignCard "DCC194" "The Thing in the Depths"

templeOfTheDeceivedCampaign :: CardDef
templeOfTheDeceivedCampaign = campaignCard "DCC197" "Temple of the Deceived"

theDrownedRuinsCampaign :: CardDef
theDrownedRuinsCampaign = campaignCard "DCC200" "The Drowned Ruins"

aStormOnCobasHavenCampaign :: CardDef
aStormOnCobasHavenCampaign = campaignCard "DCC209" "A Storm on Cobas Haven"

theCityOfCorsairsCampaign :: CardDef
theCityOfCorsairsCampaign = campaignCard "DCC215" "The City of Corsairs"

-- The Angmar Awakened Campaign Expansion

intrudersInChetwoodCampaign :: CardDef
intrudersInChetwoodCampaign = campaignCard "AAC156" "Intruders in Chetwood"

theWeatherHillsCampaign :: CardDef
theWeatherHillsCampaign = campaignCard "AAC159" "The Weather Hills"

deadmensDikeCampaign :: CardDef
deadmensDikeCampaign = campaignCard "AAC166" "Deadmen's Dike"

theWastesOfEriadorCampaign :: CardDef
theWastesOfEriadorCampaign = campaignCard "AAC171" "The Wastes of Eriador"

escapeFromMountGramCampaign :: CardDef
escapeFromMountGramCampaign = campaignCard "AAC174" "Escape from Mount Gram"

acrossTheEttenmoorsCampaign :: CardDef
acrossTheEttenmoorsCampaign = campaignCard "AAC182" "Across the Ettenmoors"

theTreacheryOfRhudaurCampaign :: CardDef
theTreacheryOfRhudaurCampaign = campaignCard "AAC184" "The Treachery of Rhudaur"

theBattleOfCarnDumCampaign :: CardDef
theBattleOfCarnDumCampaign = campaignCard "AAC188" "The Battle of Carn Dûm"

theDreadRealmCampaign :: CardDef
theDreadRealmCampaign = campaignCard "AAC193" "The Dread Realm"

-- * The Lord of the Rings Saga Expansions

aShadowOfThePastCampaign :: CardDef
aShadowOfThePastCampaign = campaignCard "141030" "A Shadow of the Past"

aKnifeInTheDarkCampaign :: CardDef
aKnifeInTheDarkCampaign = campaignCard "141044" "A Knife in the Dark"

flightToTheFordCampaign :: CardDef
flightToTheFordCampaign = campaignCard "141056" "Flight to the Ford"

theRingGoesSouthCampaign :: CardDef
theRingGoesSouthCampaign = campaignCard "142019" "The Ring Goes South"

journeyInTheDarkCampaign :: CardDef
journeyInTheDarkCampaign = campaignCard "142039" "Journey in the Dark"

breakingOfTheFellowshipCampaign :: CardDef
breakingOfTheFellowshipCampaign = campaignCard "142060" "Breaking of the Fellowship"

theUrukHaiCampaign :: CardDef
theUrukHaiCampaign = campaignCard "143071" "The Uruk-hai"

helmsDeepCampaign :: CardDef
helmsDeepCampaign = campaignCard "143077" "Helm's Deep"

theRoadToIsengardCampaign :: CardDef
theRoadToIsengardCampaign = campaignCard "143081" "The Road to Isengard"

thePassageOfTheMarshesCampaign :: CardDef
thePassageOfTheMarshesCampaign = campaignCard "144067" "The Passage of the Marshes"

journeyToTheCrossRoadsCampaign :: CardDef
journeyToTheCrossRoadsCampaign = campaignCard "144069" "Journey to the Cross-roads"

shelobsLairCampaign :: CardDef
shelobsLairCampaign = campaignCard "144073" "Shelob's Lair"

thePassingOfTheGreyCompanyCampaign :: CardDef
thePassingOfTheGreyCompanyCampaign = campaignCard "145066" "The Passing of the Grey Company"

theSiegeOfGondorCampaign :: CardDef
theSiegeOfGondorCampaign = campaignCard "145070" "The Siege of Gondor"

theBattleOfThePelennorFieldsCampaign :: CardDef
theBattleOfThePelennorFieldsCampaign = campaignCard "145078" "The Battle of the Pelennor Fields"

theTowerOfCirithUngolCampaign :: CardDef
theTowerOfCirithUngolCampaign = campaignCard "146069" "The Tower of Cirith Ungol"

theBlackGateOpensCampaign :: CardDef
theBlackGateOpensCampaign = campaignCard "146074" "The Black Gate Opens"

mountDoomCampaign :: CardDef
mountDoomCampaign = campaignCard "146079" "Mount Doom"

-- * The Ered Mithrin Campaign Expansion

journeyUpTheAnduinCampaign :: CardDef
journeyUpTheAnduinCampaign = campaignCard "EMC165" "Journey Up the Anduin"

lostInMirkwoodCampaign :: CardDef
lostInMirkwoodCampaign = campaignCard "EMC168" "Lost In Mirkwood"

theKingsQuestCampaign :: CardDef
theKingsQuestCampaign = campaignCard "EMC170" "The King's Quest"

theWitheredHeathCampaign :: CardDef
theWitheredHeathCampaign = campaignCard "EMC175" "The Withered Heath"

roamAcrossRhovanionCampaign :: CardDef
roamAcrossRhovanionCampaign = campaignCard "EMC184" "Roam Across Rhovanion"

fireInTheNightCampaign :: CardDef
fireInTheNightCampaign = campaignCard "EMC187" "Fire in the Night"

theGhostOfFramsburgCampaign :: CardDef
theGhostOfFramsburgCampaign = campaignCard "EMC189" "The Ghost of Framsburg"

mountGundabadCampaign :: CardDef
mountGundabadCampaign = campaignCard "EMC193" "Mount Gundabad"

theFateOfWilderlandCampaign :: CardDef
theFateOfWilderlandCampaign = campaignCard "EMC195" "The Fate of Wilderland"

-- * The Dark of Mirkwood

theOathCampaign :: CardDef
theOathCampaign = campaignCard "dark-of-mirkwood-050" "The Oath"

theCavesOfNibinDumCampaign :: CardDef
theCavesOfNibinDumCampaign = campaignCard "dark-of-mirkwood-051" "The Caves of Nibin-Dûm"
