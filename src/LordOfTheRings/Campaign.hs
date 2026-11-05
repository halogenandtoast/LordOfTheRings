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
import LordOfTheRings.Card (allPlayerCards)
import LordOfTheRings.Card.CardDef (CardDef, cdCardCode, cdName, cdTreasure)
import LordOfTheRings.Card.Code
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Id (ScenarioId (..))
import LordOfTheRings.Json
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Saga (FellowshipSetup (..), fellowshipSetup, hobbitScenarios)

newtype CampaignId = CampaignId {unCampaignId :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

-- | A boon or burden earned (Revised Core rulebook, "The Campaign Pool"). Whether it is a
-- boon or a burden its card says, by its trait. A player card goes to its seat's deck, an
-- encounter card into the encounter deck, or with a seat into that seat's deck once its
-- starting hand is drawn, as Recurring Nightmare; one with the Permanent keyword is
-- attached to the hero recorded with it instead.
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
  | -- | Captain Sahír has had his campaign card's attachments this scenario
    SahirKeyed
  | -- | Captain Sahír has turned over, and had what The Thing in the Depths' campaign card
    -- gives as he does
    SahirTurned
  | -- | Temple of the Deceived's secrets, one under each Temple of the Deceived location
    -- in its row of the Island Map, none once flipped
    Secrets
  | -- | the secrets flipped over
    RevealedSecrets
  | -- | the Recurring Nightmares Temple of the Deceived's resolution adds to the Campaign
    -- Pool, each for a player the first player chooses as the next scenario is set up
    RecurringNightmares
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
  | -- | the players have lost the campaign, as Mount Doom's resolution may have them: no
    -- scenario follows
    CampaignLost
  | -- | the resources on The Black Gate as The Black Gate Opens ends ('counted', by The
    -- Black Gate's code): Mount Doom's campaign card places as many on itself
    BlackGateResources
  | -- | Angmar Awakened: the books The Treachery of Rhudaur's resolution lets each player
    -- add for a Clue they control, by the seat of that player ('counted'), offered as the
    -- next scenario is set up
    BooksEarned
  | -- | The Lord of the Rings: the name and quantity of each Harad enemy underneath The
    -- Black Gate at the end of Journey to the Cross-roads ('counted', by card code), which
    -- The Battle of the Pelennor Fields sets aside
    HaradEnemies
  | -- | The Lord of the Rings: the resources on The Corsair Fleet at the end of The Siege of
    -- Gondor ('counted'), placed on it as The Battle of the Pelennor Fields is set up
    CorsairFleetResources
  | -- | The Lord of the Rings: the heroes taken captive at the end of Breaking of the
    -- Fellowship, one for each player
    Captives
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
  , campaignLogCounts :: Map CampaignLogKey (Map CardCode Int)
  -- ^ numbers noted for cards, as the damage on Protect the Innocent
  , campaignLogSeatThreatPenalty :: Map Int Int
  -- ^ a starting threat penalty of one seat's alone, as The Passage of the Marshes'
  -- resolution gives a player who chooses it
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
  parseJSON = genericParseJSON (aesonOptions $ Just "campaignLog") . withDefaults [("xp", Number 0), ("marked", object []), ("cardResources", Number 0), ("counts", object []), ("seatThreatPenalty", object [])]

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

instance HasField "counts" CampaignLog (Map CampaignLogKey (Map CardCode Int)) where
  getField = campaignLogCounts

instance HasField "seatThreatPenalty" CampaignLog (Map Int Int) where
  getField = campaignLogSeatThreatPenalty

-- | The seat's starting threat penalty: the campaign's and its own.
seatThreatPenalty :: Int -> CampaignLog -> Int
seatThreatPenalty seat l = l.threatPenalty + Map.findWithDefault 0 seat l.seatThreatPenalty

-- | A campaign: its scenarios in order, each with its campaign card, The Hobbit's having
-- none.
data CampaignDef = CampaignDef
  { campaignDefId :: CampaignId
  , campaignDefName :: Text
  , campaignDefScenarios :: [(ScenarioId, Maybe CardDef)]
  }

-- | The Revised Core Set's campaign, Mirkwood Paths.
revisedCore :: CampaignDef
revisedCore =
  CampaignDef
    "revised-core"
    "Mirkwood Paths"
    [ ("passage-through-mirkwood", Just Cards.passageThroughMirkwoodCampaign)
    , ("journey-along-the-anduin", Just Cards.journeyAlongTheAnduinCampaign)
    , ("escape-from-dol-guldur", Just Cards.escapeFromDolGuldurCampaign)
    ]

-- | The Dream-chaser Campaign Expansion: The Grey Havens' three scenarios and the
-- Dream-chaser cycle's six, one campaign.
dreamChaser :: CampaignDef
dreamChaser =
  CampaignDef
    "dream-chaser"
    "The Dream-chaser Campaign"
    [ ("voyage-across-belegaer", Just Cards.voyageAcrossBelegaerCampaign)
    , ("the-fate-of-numenor", Just Cards.theFateOfNumenorCampaign)
    , ("raid-on-the-grey-havens", Just Cards.raidOnTheGreyHavensCampaign)
    , ("flight-of-the-stormcaller", Just Cards.flightOfTheStormcallerCampaign)
    , ("the-thing-in-the-depths", Just Cards.theThingInTheDepthsCampaign)
    , ("temple-of-the-deceived", Just Cards.templeOfTheDeceivedCampaign)
    , ("the-drowned-ruins", Just Cards.theDrownedRuinsCampaign)
    , ("a-storm-on-cobas-haven", Just Cards.aStormOnCobasHavenCampaign)
    , ("the-city-of-corsairs", Just Cards.theCityOfCorsairsCampaign)
    ]


-- | The Angmar Awakened Campaign Expansion: The Lost Realm's scenarios and the Angmar
-- Awakened cycle's, one campaign.
angmarAwakened :: CampaignDef
angmarAwakened =
  CampaignDef
    "angmar-awakened"
    "Angmar Awakened"
    [ ("intruders-in-chetwood", Just Cards.intrudersInChetwoodCampaign)
    , ("the-weather-hills", Just Cards.theWeatherHillsCampaign)
    , ("deadmens-dike", Just Cards.deadmensDikeCampaign)
    , ("the-wastes-of-eriador", Just Cards.theWastesOfEriadorCampaign)
    , ("escape-from-mount-gram", Just Cards.escapeFromMountGramCampaign)
    , ("across-the-ettenmoors", Just Cards.acrossTheEttenmoorsCampaign)
    , ("the-treachery-of-rhudaur", Just Cards.theTreacheryOfRhudaurCampaign)
    , ("the-battle-of-carn-dum", Just Cards.theBattleOfCarnDumCampaign)
    , ("the-dread-realm", Just Cards.theDreadRealmCampaign)
    ]

-- | The Hobbit Saga Expansions' six scenarios, played in order with the same heroes, the
-- treasures each player discovers carried to the later ones (The Hobbit: Over Hill and
-- Under Hill rules insert, "Treasure").
theHobbit :: CampaignDef
theHobbit = CampaignDef "the-hobbit" "The Hobbit" [(sid, Nothing) | sid <- hobbitScenarios]

-- | The Lord of the Rings Saga Expansions' eighteen scenarios, one campaign (The Black
-- Riders rules insert, "Campaign Mode").
theLordOfTheRings :: CampaignDef
theLordOfTheRings =
  CampaignDef
    "the-lord-of-the-rings"
    "The Lord of the Rings"
    [ ("a-shadow-of-the-past", Just Cards.aShadowOfThePastCampaign)
    , ("a-knife-in-the-dark", Just Cards.aKnifeInTheDarkCampaign)
    , ("flight-to-the-ford", Just Cards.flightToTheFordCampaign)
    , ("the-ring-goes-south", Just Cards.theRingGoesSouthCampaign)
    , ("journey-in-the-dark", Just Cards.journeyInTheDarkCampaign)
    , ("breaking-of-the-fellowship", Just Cards.breakingOfTheFellowshipCampaign)
    , ("the-uruk-hai", Just Cards.theUrukHaiCampaign)
    , ("helms-deep", Just Cards.helmsDeepCampaign)
    , ("the-road-to-isengard", Just Cards.theRoadToIsengardCampaign)
    , ("the-passage-of-the-marshes", Just Cards.thePassageOfTheMarshesCampaign)
    , ("journey-to-the-cross-roads", Just Cards.journeyToTheCrossRoadsCampaign)
    , ("shelobs-lair", Just Cards.shelobsLairCampaign)
    , ("the-passing-of-the-grey-company", Just Cards.thePassingOfTheGreyCompanyCampaign)
    , ("the-siege-of-gondor", Just Cards.theSiegeOfGondorCampaign)
    , ("the-battle-of-the-pelennor-fields", Just Cards.theBattleOfThePelennorFieldsCampaign)
    , ("the-tower-of-cirith-ungol", Just Cards.theTowerOfCirithUngolCampaign)
    , ("the-black-gate-opens", Just Cards.theBlackGateOpensCampaign)
    , ("mount-doom", Just Cards.mountDoomCampaign)
    ]

allCampaigns :: [CampaignDef]
allCampaigns = [revisedCore, angmarAwakened, dreamChaser, theHobbit, theLordOfTheRings]

lookupCampaign :: CampaignId -> Maybe CampaignDef
lookupCampaign cid = find ((== cid) . campaignDefId) allCampaigns

newCampaignLog :: CampaignId -> CampaignLog
newCampaignLog cid = CampaignLog cid [] [] 0 [] mempty [] 0 mempty 0 mempty mempty

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

-- | The checkboxes by each title on the Dream-chaser Campaign Log sheet (Dream-chaser
-- Campaign Expansion rules, p. 31).
xpCards :: CampaignLog -> [XpCard]
xpCards l = case l.campaign of
  "dream-chaser" ->
    [upgrade code n | (code, n) <- upgradable]
      -- the Grey Havens' and the cycle's own cards, their Upgraded sides the expansion's
      <> [XpCard "11080" 4 (Just "DCC136U"), XpCard "11081" 4 (Just "DCC137U"), XpCard "11082" 4 (Just "DCC140U"), XpCard "11083" 4 (Just "DCC141U"), XpCard "12071" 2 (Just "DCC078U")]
      <> [XpCard code 5 Nothing | code <- scalable]
      <> [XpCard code 4 Nothing | code <- command]
      -- Send for Help: Leadership 3, Tactics 2, Spirit 4, Lore 3
      <> [XpCard "DCC211" 3 Nothing, XpCard "DCC212" 2 Nothing, XpCard "DCC213" 4 Nothing, XpCard "DCC214" 3 Nothing]
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
  -- Galdor's Counsel, Come Together, Starla and Círdan's Blessing
  scalable = ["DCC184", "DCC185", "DCC186", "DCC187", "DCC195", "DCC198", "DCC205", "DCC206", "DCC207", "DCC208"]
  command = ["DCC201", "DCC202", "DCC203", "DCC204"]

marked :: CardCode -> CampaignLog -> Int
marked code = Map.findWithDefault 0 code . (.marked)

-- | A scalable card's level: the boxes marked by its title, 0 with none ("A scalable
-- card is leveled up for each checkbox marked by its title").
level :: CardCode -> CampaignLog -> Int
level = marked

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
  guard $ not (Map.member CampaignLost l.notes)
  def <- lookupCampaign l.campaign
  headMay [sid | (sid, _) <- campaignDefScenarios def, sid `notElem` map completedScenario l.completed]

-- | The campaign card set next to the quest deck while the scenario is played.
campaignCardFor :: CampaignLog -> ScenarioId -> Maybe CardCode
campaignCardFor l sid = cdCardCode <$> (join . lookup sid . campaignDefScenarios =<< lookupCampaign l.campaign)

{- | The heroes each seat brings to the next scenario, checked against the log: none
fallen, and each hero traded for one of another title, or a fallen hero replaced, a
permanent +1 to the threat penalty (Revised Core rulebook, "The Fellowship of Heroes").
Heroes are told by title: another version of the same hero, a trade within the group or a
hero added where none left costs nothing, and no version of a fallen hero may come back
(The Road Darkens rules insert, FAQ). The first scenario records them. Gives back the
names of any fallen heroes brought.

Where the first player takes control of the Fellowship sphere Aragorn, the Aragorn a seat
recorded leaves it, replaced for free, his Permanent cards going to the Fellowship
Aragorn; a fallen Aragorn comes off the list of fallen heroes, at a +1 penalty (The
Treason of Saruman rules insert, "Campaign Mode Rules - Aragorn").
-}
seatHeroes :: [[CardCode]] -> CampaignLog -> Either [CardCode] CampaignLog
seatHeroes seats l
  | notNull refused = Left refused
  | otherwise =
      Right
        l
          { campaignLogHeroes = seats
          , campaignLogThreatPenalty = l.threatPenalty + changed + (if restored then 1 else 0)
          , campaignLogFallenHeroes = filter (\c -> not (restored && title c == "Aragorn")) l.fallenHeroes
          , campaignLogPool = [maybe p (\a -> p {poolCardAttachedTo = Just a}) (guard (maybe False ((== "Aragorn") . title) p.attachedTo) *> fellowshipAragorn) | p <- l.pool]
          }
 where
  title code = maybe "" (\d -> let Name t _ = cdName d in t) (Map.lookup code allPlayerCards)
  next = nextScenario l
  fellowshipAragorn = do
    fs <- fellowshipSetup =<< next
    code <- headMay fs.fellowshipHeroes
    guard $ title code == "Aragorn"
    pure code
  swapsAragorn = isJust fellowshipAragorn
  restored = swapsAragorn && "Aragorn" `elem` map title l.fallenHeroes
  fallenTitles = map title l.fallenHeroes
  refused = filter (\c -> title c `elem` fallenTitles || c `elem` lockedHeroes l) (concat seats)
  oldTitles = ordNub [t | t <- map title (concat l.heroes), not (swapsAragorn && t == "Aragorn")]
  newTitles = ordNub $ map title (concat seats)
  -- the first scenario, with no heroes recorded yet, changes none; nor does a scenario
  -- whose campaign card lets heroes change freely
  changed
    | maybe False (`elem` freeHeroChange) next = 0
    | otherwise = min (length (filter (`notElem` newTitles) oldTitles)) (length (filter (`notElem` oldTitles) newTitles))

-- | The scenarios whose campaign card reads "Each player may change hero cards without
-- incurring the +1 threat penalty", The Passage of the Marshes' as the FAQ's errata has it.
freeHeroChange :: [ScenarioId]
freeHeroChange =
  [ "the-wastes-of-eriador"
  , "the-battle-of-carn-dum"
  , "the-ring-goes-south"
  , "helms-deep"
  , "the-passage-of-the-marshes"
  , "the-passing-of-the-grey-company"
  , "the-tower-of-cirith-ungol"
  , "the-black-gate-opens"
  , "mount-doom"
  ]

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
seatDeck seat l cards = filter (keeps . fst) cards <> [(c, 1) | c <- seatPoolCards seat l]
 where
  -- no version of a fallen hero may be used, its ally versions included (The Road Darkens
  -- rules insert, FAQ)
  fallenTitles = mapMaybe titleOf l.fallenHeroes
  titleOf code = (\d -> let Name t _ = cdName d in t) <$> Map.lookup code allPlayerCards
  keeps code = code `notElem` recorded LostInMountGram l && maybe True (`notElem` fallenTitles) (titleOf code)

-- | Each seat's boons and burdens that are player cards, to shuffle into its deck: those
-- not attached to a hero. (An encounter card for its deck the scenario shuffles in, and a
-- treasure its player chooses to add as the scenario is set up.)
seatPoolCards :: Int -> CampaignLog -> [CardCode]
seatPoolCards seat l = [p.code | p <- l.pool, p.seat == Just seat, isNothing p.attachedTo, Just def <- [Map.lookup p.code allPlayerCards], not (cdTreasure def)]

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
  | -- | the hero goes on the list of fallen heroes, as Fallen into Evil's does
    AddFallenHero CardCode
  | -- | the hero comes off it, as Beyond All Hope's, or The Black Gate Opens' resolution
    -- keeps a hero off it
    RemoveFallenHero CardCode
  | -- | a permanent starting threat penalty for the seat alone
    AdjustSeatThreatPenalty Int Int
  | -- | the seat's recorded heroes: the first taken off, if one is given, the second put on,
    -- the change at no threat penalty, as a hero Beyond All Hope brings back from the
    -- fallen joins its player (The Uruk-hai's resolution)
    ReplaceSeatHero Int (Maybe CardCode) CardCode
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

updateLog :: CampaignLogUpdate -> CampaignLog -> CampaignLog
updateLog = \case
  AddToPool p -> addToPool p
  RemoveFromPool code -> \l -> l {campaignLogPool = filter ((/= code) . (.code)) l.pool}
  AdjustThreatPenalty n -> \l -> l {campaignLogThreatPenalty = l.threatPenalty + n}
  Record key cards -> record key cards
  Unrecord key -> \l -> l {campaignLogNotes = Map.delete key l.notes, campaignLogCounts = Map.delete key l.counts}
  AddFallenHero code -> recordFallen [code]
  RemoveFallenHero code -> \l -> l {campaignLogFallenHeroes = filter (/= code) l.fallenHeroes}
  AdjustSeatThreatPenalty seat n -> \l -> l {campaignLogSeatThreatPenalty = Map.insertWith (+) seat n l.seatThreatPenalty}
  ReplaceSeatHero seat old new -> \l -> l {campaignLogHeroes = [if i == seat then filter ((/= old) . Just) hs <> [new] else hs | (i, hs) <- zip [0 ..] l.heroes]}
