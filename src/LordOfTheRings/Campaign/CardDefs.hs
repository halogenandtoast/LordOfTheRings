module LordOfTheRings.Campaign.CardDefs (module LordOfTheRings.Campaign.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.CardType
import LordOfTheRings.Card.Code
import LordOfTheRings.Name (Name)
import LordOfTheRings.Prelude

-- | The campaign cards: each sits next to the quest deck in campaign mode, its setup
-- followed as the scenario is set up and the resolution on its back once it is won. What
-- they say is their scenario's to do; see "LordOfTheRings.Campaign".
allCampaignCards :: Map CardCode CardDef
allCampaignCards =
  Map.fromList
    $ map
      (\def -> (cdCardCode def, def))
      [ passageThroughMirkwoodCampaign
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
