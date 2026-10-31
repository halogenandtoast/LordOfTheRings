{- | Campaign mode (Revised Core rulebook, "Campaign Mode"): the scenarios of a campaign
are played in order, each one only once the one before it has been won, and a campaign
log carries from one to the next.

As in Arkham, the log is a value the scenarios read and write: the server keeps it with
the campaign between games and hands it to each game it deals ('scenarioCampaign'). What
a campaign card says is its scenario's to do, as Arkham's scenarios record their
resolutions: its setup as the scenario sets up, its resolution on 'ResolveCampaign',
which the game runs once the scenario is won. What every campaign does alike, the threat
penalty, the Campaign Pool and the fallen heroes, the scenario runner does.
-}
module LordOfTheRings.Campaign (module LordOfTheRings.Campaign) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Campaign.CardDefs qualified as Cards
import LordOfTheRings.Card.CardDef (CardDef, cdCardCode)
import LordOfTheRings.Card.Code
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Id (ScenarioId (..))
import LordOfTheRings.Json
import LordOfTheRings.Prelude

newtype CampaignId = CampaignId {unCampaignId :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

-- | A boon or burden earned (Revised Core rulebook, "The Campaign Pool"). Whether it is a
-- boon or a burden its card says, by its trait. A player card goes to its seat's deck, an
-- encounter card into the encounter deck; one with the Permanent keyword is attached to
-- the hero recorded with it instead.
data PoolCard = PoolCard
  { poolCardCode :: CardCode
  , poolCardSeat :: Maybe Int
  -- ^ the seat whose deck a player card goes to, as "each player earns 1 copy" gives one
  , poolCardAttachedTo :: Maybe CardCode
  -- ^ the hero a Permanent card is attached to, as the notes record it
  }
  deriving stock (Show, Eq, Generic)

-- | What a campaign notes in the log, by name, as Arkham's 'CampaignLogKey': the cards
-- recorded under it.
data CampaignLogKey
  = -- | the hero taken prisoner at the start of Escape from Dol Guldur (Journey Along the
    -- Anduin's resolution)
    Prisoner
  | -- | Mendor, added to the Campaign Pool for the next campaign (Escape from Dol
    -- Guldur's resolution)
    MendorJoinsTheNextCampaign
  | -- | Angmar Awakened: the damage on Protect the Innocent, later Arnor Ravaged, as its
    -- campaign cards note it ('counted')
    ProtectTheInnocentDamage
  | -- | Angmar Awakened: the characters "determined", by Determination (The Wastes of Eriador)
    Determined
  | -- | Angmar Awakened: the unique cards left in a captured deck at the end of Escape from
    -- Mount Gram, which no deck may include for the rest of the campaign
    LostInMountGram
  | -- | Angmar Awakened: the damage on each hero at the end of The Battle of Carn Dûm
    -- ('counted'), each hero beginning The Dread Realm with as much
    HeroDamage
  | -- | Angmar Awakened: Deadmen's Dike's resolution has the players choose Iârion's
    -- Pendant or Amarthiúl's Courage, which they do as the next scenario is set up
    ChoosePendantOrCourage
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

-- | Keyed by name in JSON, as the notes the frontend shows.
instance ToJSONKey CampaignLogKey where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey CampaignLogKey where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

data CompletedScenario = CompletedScenario
  { completedScenario :: ScenarioId
  , completedScore :: Int
  }
  deriving stock (Show, Eq, Generic)

-- | The Campaign Log, as printed on pages 32-33 of the Revised Core rulebook.
data CampaignLog = CampaignLog
  { campaignLogCampaign :: CampaignId
  , campaignLogHeroes :: [[CardCode]]
  -- ^ each seat's heroes, in seat order; none until the first scenario is dealt
  , campaignLogFallenHeroes :: [CardCode]
  , campaignLogThreatPenalty :: Int
  , campaignLogPool :: [PoolCard]
  , campaignLogNotes :: Map CampaignLogKey [CardCode]
  , campaignLogCompleted :: [CompletedScenario]
  , campaignLogCounts :: Map CampaignLogKey (Map CardCode Int)
  -- ^ numbers noted for cards, as the damage on Protect the Innocent
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PoolCard where
  toJSON = genericToJSON (aesonOptions $ Just "poolCard")

instance FromJSON PoolCard where
  parseJSON = genericParseJSON (aesonOptions $ Just "poolCard")

instance ToJSON CompletedScenario where
  toJSON = genericToJSON (aesonOptions $ Just "completed")

instance FromJSON CompletedScenario where
  parseJSON = genericParseJSON (aesonOptions $ Just "completed")

instance ToJSON CampaignLog where
  toJSON = genericToJSON (aesonOptions $ Just "campaignLog")

instance FromJSON CampaignLog where
  parseJSON = genericParseJSON (aesonOptions $ Just "campaignLog") . withDefaults [("counts", object [])]

instance HasField "campaign" CampaignLog CampaignId where
  getField = campaignLogCampaign

instance HasField "heroes" CampaignLog [[CardCode]] where
  getField = campaignLogHeroes

instance HasField "fallenHeroes" CampaignLog [CardCode] where
  getField = campaignLogFallenHeroes

instance HasField "threatPenalty" CampaignLog Int where
  getField = campaignLogThreatPenalty

instance HasField "pool" CampaignLog [PoolCard] where
  getField = campaignLogPool

instance HasField "notes" CampaignLog (Map CampaignLogKey [CardCode]) where
  getField = campaignLogNotes

instance HasField "completed" CampaignLog [CompletedScenario] where
  getField = campaignLogCompleted

instance HasField "counts" CampaignLog (Map CampaignLogKey (Map CardCode Int)) where
  getField = campaignLogCounts

-- | A campaign: its scenarios in order, each with its campaign card.
data CampaignDef = CampaignDef
  { campaignDefId :: CampaignId
  , campaignDefName :: Text
  , campaignDefScenarios :: [(ScenarioId, CardDef)]
  }

-- | The Revised Core Set's campaign, Mirkwood Paths.
revisedCore :: CampaignDef
revisedCore =
  CampaignDef
    "revised-core"
    "Mirkwood Paths"
    [ ("passage-through-mirkwood", Cards.passageThroughMirkwoodCampaign)
    , ("journey-along-the-anduin", Cards.journeyAlongTheAnduinCampaign)
    , ("escape-from-dol-guldur", Cards.escapeFromDolGuldurCampaign)
    ]

-- | The Angmar Awakened Campaign Expansion: The Lost Realm's scenarios and the Angmar
-- Awakened cycle's, one campaign.
angmarAwakened :: CampaignDef
angmarAwakened =
  CampaignDef
    "angmar-awakened"
    "Angmar Awakened"
    [ ("intruders-in-chetwood", Cards.intrudersInChetwoodCampaign)
    , ("the-weather-hills", Cards.theWeatherHillsCampaign)
    , ("deadmens-dike", Cards.deadmensDikeCampaign)
    , ("the-wastes-of-eriador", Cards.theWastesOfEriadorCampaign)
    , ("escape-from-mount-gram", Cards.escapeFromMountGramCampaign)
    , ("across-the-ettenmoors", Cards.acrossTheEttenmoorsCampaign)
    , ("the-treachery-of-rhudaur", Cards.theTreacheryOfRhudaurCampaign)
    , ("the-battle-of-carn-dum", Cards.theBattleOfCarnDumCampaign)
    , ("the-dread-realm", Cards.theDreadRealmCampaign)
    ]

allCampaigns :: [CampaignDef]
allCampaigns = [revisedCore, angmarAwakened]

lookupCampaign :: CampaignId -> Maybe CampaignDef
lookupCampaign cid = find ((== cid) . campaignDefId) allCampaigns

newCampaignLog :: CampaignId -> CampaignLog
newCampaignLog cid = CampaignLog cid [] [] 0 [] mempty [] mempty

-- | The scenario to play next: the first not yet won. Nothing once the campaign is over.
nextScenario :: CampaignLog -> Maybe ScenarioId
nextScenario l = do
  def <- lookupCampaign l.campaign
  headMay [sid | (sid, _) <- campaignDefScenarios def, sid `notElem` map completedScenario l.completed]

-- | The campaign card set next to the quest deck while the scenario is played.
campaignCardFor :: CampaignLog -> ScenarioId -> Maybe CardCode
campaignCardFor l sid = cdCardCode <$> (lookup sid . campaignDefScenarios =<< lookupCampaign l.campaign)

{- | The heroes each seat brings to the next scenario, checked against the log: none
fallen, and each hero not the one the seat had before a permanent +1 to the threat
penalty, a fallen hero replaced or one traded for another (Revised Core rulebook, "The
Fellowship of Heroes"). The first scenario records them. Gives back the names of any
fallen heroes brought.
-}
seatHeroes :: [[CardCode]] -> CampaignLog -> Either [CardCode] CampaignLog
seatHeroes seats l
  | notNull refused = Left refused
  | otherwise = Right l {campaignLogHeroes = seats, campaignLogThreatPenalty = l.threatPenalty + changed}
 where
  refused = filter (\c -> c `elem` l.fallenHeroes || c `elem` lockedHeroes l) (concat seats)
  -- the first scenario, with no heroes recorded yet, changes none; nor does a seat the
  -- log did not have; nor a scenario whose campaign card lets heroes change freely
  changed
    | maybe False (`elem` freeHeroChange) (nextScenario l) = 0
    | otherwise = sum [length (filter (`notElem` old) new) | (old, new) <- zip l.heroes seats]

-- | The scenarios whose campaign card reads "Each player may change hero cards without
-- incurring the +1 threat penalty" (The Wastes of Eriador, The Battle of Carn Dûm).
freeHeroChange :: [ScenarioId]
freeHeroChange = ["the-wastes-of-eriador", "the-battle-of-carn-dum"]

-- | Heroes the campaign does not allow yet: the Amarthiúl hero, in Angmar Awakened, until
-- The Battle of Carn Dûm's campaign card lets him in.
lockedHeroes :: CampaignLog -> [CardCode]
lockedHeroes l
  | l.campaign == "angmar-awakened"
  , "the-battle-of-carn-dum" `notElem` map completedScenario l.completed
  , nextScenario l /= Just "the-battle-of-carn-dum" =
      [cdCardCode Heroes.amarthiul]
  | otherwise = []

-- | A seat's deck as the campaign deals it: its boons and burdens that are player cards
-- added, and the cards the log bars from every deck left out.
seatDeck :: Int -> CampaignLog -> [(CardCode, Int)] -> [(CardCode, Int)]
seatDeck seat l cards = filter ((`notElem` recorded LostInMountGram l) . fst) cards <> [(c, 1) | c <- seatPoolCards seat l]

-- | Each seat's boons and burdens that are player cards, to shuffle into its deck: those
-- not attached to a hero.
seatPoolCards :: Int -> CampaignLog -> [CardCode]
seatPoolCards seat l = [p.code | p <- l.pool, p.seat == Just seat, isNothing p.attachedTo]

instance HasField "code" PoolCard CardCode where
  getField = poolCardCode

instance HasField "seat" PoolCard (Maybe Int) where
  getField = poolCardSeat

instance HasField "attachedTo" PoolCard (Maybe CardCode) where
  getField = poolCardAttachedTo

-- | Heroes in their owners' discard piles at the end of a scenario fall: recorded as
-- Fallen Heroes, the Permanent boons and burdens on them leave the Campaign Pool.
recordFallen :: [CardCode] -> CampaignLog -> CampaignLog
recordFallen heroes l =
  l
    { campaignLogFallenHeroes = l.fallenHeroes <> filter (`notElem` l.fallenHeroes) heroes
    , campaignLogPool = filter (maybe True (`notElem` heroes) . (.attachedTo)) l.pool
    }

-- | Earn a boon or burden.
addToPool :: PoolCard -> CampaignLog -> CampaignLog
addToPool p l = l {campaignLogPool = l.pool <> [p]}

record :: CampaignLogKey -> [CardCode] -> CampaignLog -> CampaignLog
record key cards l = l {campaignLogNotes = Map.insert key cards l.notes}

recorded :: CampaignLogKey -> CampaignLog -> [CardCode]
recorded key = Map.findWithDefault [] key . (.notes)

-- | Note a number for a card, replacing the one noted before.
recordCount :: CampaignLogKey -> CardCode -> Int -> CampaignLog -> CampaignLog
recordCount key code n l = l {campaignLogCounts = Map.insertWith (<>) key (Map.singleton code n) l.counts}

recordedCount :: CampaignLogKey -> CardCode -> CampaignLog -> Int
recordedCount key code = Map.findWithDefault 0 code . Map.findWithDefault mempty key . (.counts)

-- | What a card writes to the log while its scenario is played, as Mysterious Omens' boons
-- or Fend Off Despair's threat. Kept if the scenario is won, as the rest of its log.
data CampaignLogUpdate
  = AddToPool PoolCard
  | RemoveFromPool CardCode
  | AdjustThreatPenalty Int
  | Record CampaignLogKey [CardCode]
  | Unrecord CampaignLogKey
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

updateLog :: CampaignLogUpdate -> CampaignLog -> CampaignLog
updateLog = \case
  AddToPool p -> addToPool p
  RemoveFromPool code -> \l -> l {campaignLogPool = filter ((/= code) . (.code)) l.pool}
  AdjustThreatPenalty n -> \l -> l {campaignLogThreatPenalty = l.threatPenalty + n}
  Record key cards -> record key cards
  Unrecord key -> \l -> l {campaignLogNotes = Map.delete key l.notes}
