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

import Data.List (elemIndex)
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
  | -- | Graxar was in the victory display at the end of Raid on the Grey Havens: Parrying
    -- Cutlass or Throwing Axe is chosen, and its hero, as the next scenario is set up
    GraxarDefeated
  | -- | the boons each player chose during this scenario's setup, as Galdor's Counsel: they
    -- add 1 to Ominous Fate's X, and join the Campaign Pool once the scenario is won
    ChosenDuringSetup
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
  , campaignLogXp :: Int
  -- ^ experience not yet spent (Dream-chaser Campaign Rules, "Gaining XP")
  , campaignLogMarked :: Map CardCode Int
  -- ^ the checkboxes marked by each scalable or upgradable card's title
  , campaignLogCardResources :: Int
  -- ^ the resources on this scenario's campaign card, as Revenant places them: counted for
  -- XP as it is resolved, none carried to the next
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

-- | A log kept before XP was recorded has none.
instance FromJSON CampaignLog where
  parseJSON = genericParseJSON (aesonOptions $ Just "campaignLog") . withDefaults [("xp", Number 0), ("marked", object []), ("cardResources", Number 0)]

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

instance HasField "xp" CampaignLog Int where
  getField = campaignLogXp

instance HasField "marked" CampaignLog (Map CardCode Int) where
  getField = campaignLogMarked

instance HasField "cardResources" CampaignLog Int where
  getField = campaignLogCardResources

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

-- | The Dream-chaser Campaign Expansion: The Grey Havens' three scenarios and the
-- Dream-chaser cycle's six, one campaign.
dreamChaser :: CampaignDef
dreamChaser =
  CampaignDef
    "dream-chaser"
    "The Dream-chaser Campaign"
    [ ("voyage-across-belegaer", Cards.voyageAcrossBelegaerCampaign)
    , ("the-fate-of-numenor", Cards.theFateOfNumenorCampaign)
    , ("raid-on-the-grey-havens", Cards.raidOnTheGreyHavensCampaign)
    , ("flight-of-the-stormcaller", Cards.flightOfTheStormcallerCampaign)
    , ("the-thing-in-the-depths", Cards.theThingInTheDepthsCampaign)
    , ("temple-of-the-deceived", Cards.templeOfTheDeceivedCampaign)
    , ("the-drowned-ruins", Cards.theDrownedRuinsCampaign)
    , ("a-storm-on-cobas-haven", Cards.aStormOnCobasHavenCampaign)
    , ("the-city-of-corsairs", Cards.theCityOfCorsairsCampaign)
    ]

allCampaigns :: [CampaignDef]
allCampaigns = [revisedCore, dreamChaser]

lookupCampaign :: CampaignId -> Maybe CampaignDef
lookupCampaign cid = find ((== cid) . campaignDefId) allCampaigns

newCampaignLog :: CampaignId -> CampaignLog
newCampaignLog cid = CampaignLog cid [] [] 0 [] mempty [] 0 mempty 0

-- | The scenario's part number in its campaign, from 1, as Ominous Fate's X reads it.
partNumber :: CampaignLog -> ScenarioId -> Int
partNumber l sid = maybe 0 (+ 1) $ (elemIndex sid . map fst . campaignDefScenarios) =<< lookupCampaign l.campaign

{- | What XP is spent on (Dream-chaser Campaign Rules, "Scalable & Upgradable Cards"):
each card with as many checkboxes as its upgrade costs XP, the card upgraded once all
are marked, its Upgraded side used from then on; a scalable card levels up with each.
-}
data XpCard = XpCard
  { xpCard :: CardCode
  , xpBoxes :: Int
  , xpUpgraded :: Maybe CardCode
  -- ^ the Upgraded side, once every box is marked
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON XpCard where
  toJSON = genericToJSON (aesonOptions $ Just "xp")

-- ponytail: the Campaign Log sheet's checkbox counts are not to hand; an upgradable card
-- has as many as the XP cost printed on its Upgraded side, a scalable card 5.
xpCards :: CampaignLog -> [XpCard]
xpCards l = case l.campaign of
  "dream-chaser" ->
    [upgrade code n | (code, n) <- upgradable]
      -- the Grey Havens' and the cycle's own cards, their Upgraded sides the expansion's
      <> [XpCard "11080" 4 (Just "DCC136U"), XpCard "11081" 4 (Just "DCC137U"), XpCard "11082" 4 (Just "DCC140U"), XpCard "11083" 4 (Just "DCC141U"), XpCard "12071" 2 (Just "DCC078U")]
      <> [XpCard code 5 Nothing | code <- scalable]
      <> [XpCard "DCC211" 3 Nothing, XpCard "DCC212" 3 Nothing, XpCard "DCC213" 3 Nothing, XpCard "DCC214" 3 Nothing]
      -- "Gavin returned", once A Storm on Cobas Haven is won
      <> [XpCard "DCC210" 4 Nothing | "a-storm-on-cobas-haven" `elem` map completedScenario l.completed]
  _ -> []
 where
  upgrade code n = XpCard code n (Just $ CardCode $ unCardCode code <> "U")
  upgradable =
    [ ("DCC172", 2), ("DCC173", 2), ("DCC174", 2), ("DCC175", 2)
    , ("DCC180", 3), ("DCC183", 4), ("DCC189", 5), ("DCC190", 5)
    , ("DCC192", 5), ("DCC193", 5)
    ]
  -- Galdor's Counsel, Come Together, Starla, Command and Círdan's Blessing
  scalable = ["DCC184", "DCC185", "DCC186", "DCC187", "DCC195", "DCC198", "DCC201", "DCC202", "DCC203", "DCC204", "DCC205", "DCC206", "DCC207", "DCC208"]

marked :: CardCode -> CampaignLog -> Int
marked code = Map.findWithDefault 0 code . (.marked)

-- | A scalable card's level: the boxes marked by its title.
-- ponytail: a card with none marked plays at level 1, so a boon taken before XP is spent
-- on it still does something.
level :: CardCode -> CampaignLog -> Int
level code l = max 1 (marked code l)

isUpgraded :: CardCode -> CampaignLog -> Bool
isUpgraded code l = any (\x -> xpCard x == code && marked code l >= xpBoxes x) (xpCards l)

-- | The side of the card to use: its Upgraded side once upgraded.
upgradedCode :: CampaignLog -> CardCode -> CardCode
upgradedCode l code = fromMaybe code $ headMay [u | x <- xpCards l, xpCard x == code, Just u <- [xpUpgraded x], isUpgraded code l]

{- | Spend 1 XP to mark one of the card's checkboxes. Its Upgraded side replaces it in
the Campaign Pool once every box is marked. Nothing if there is no XP left or no box.
-}
spendXp :: CardCode -> CampaignLog -> Maybe CampaignLog
spendXp code l = do
  x <- find ((== code) . xpCard) (xpCards l)
  guard $ l.xp > 0 && marked code l < xpBoxes x
  let l' = l {campaignLogXp = l.xp - 1, campaignLogMarked = Map.insertWith (+) code 1 l.marked}
  pure l' {campaignLogPool = [p {poolCardCode = upgradedCode l' p.code} | p <- l'.pool]}

gainXp :: Int -> CampaignLog -> CampaignLog
gainXp n l = l {campaignLogXp = max 0 (l.xp + n)}

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
