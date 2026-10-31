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
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

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
  parseJSON = genericParseJSON (aesonOptions $ Just "campaignLog")

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

allCampaigns :: [CampaignDef]
allCampaigns = [revisedCore]

lookupCampaign :: CampaignId -> Maybe CampaignDef
lookupCampaign cid = find ((== cid) . campaignDefId) allCampaigns

newCampaignLog :: CampaignId -> CampaignLog
newCampaignLog cid = CampaignLog cid [] [] 0 [] mempty []

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
  | notNull fallen = Left fallen
  | otherwise = Right l {campaignLogHeroes = seats, campaignLogThreatPenalty = l.threatPenalty + changed}
 where
  fallen = filter (`elem` l.fallenHeroes) (concat seats)
  -- the first scenario, with no heroes recorded yet, changes none; nor does a seat the
  -- log did not have
  changed = sum [length (filter (`notElem` old) new) | (old, new) <- zip l.heroes seats]

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
