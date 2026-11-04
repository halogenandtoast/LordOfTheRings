{- | What The Lonely Mountain's cards share: burgle attempts and the burgle effects printed
on its encounter cards (The Hobbit: On the Doorstep rules insert, "Burgle"), Smaug the
Golden's progress, the treasures stacked under The Lonely Mountain, and the jump to stage
4A. The scenario runs the burgle attempt itself (see
"LordOfTheRings.Scenario.Scenarios.TheLonelyMountain").
-}
module LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (
  Burgle (..),
  BurgleMatch (..),
  burgleOf,
  withBurgle,
  hasBurgle,
  burgleMatches,
  burgleQuestionKey,
  makeBurgleAttempt,
  burgled,
  lonelyMountainTreasures,
  getSmaugTheGolden,
  getSmaugTheMagnificent,
  getTheLonelyMountain,
  enemyProgress,
  placeProgressOnSmaugTheGolden,
  advanceToStageFour,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (Card, CardCode, CardCost (StaticCost), CardDef (..), HasCardCode, toCardCode, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push, pushAll)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyTokens))
import LordOfTheRings.Id
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Log.Block (LogBlockKind (BurgleBlock))
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioQuestDeck))
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token (Token (Progress), countTokens)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

-- | What a card in hand must share with the revealed card.
data BurgleMatch = BySphere | ByType | ByCost
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | A burgle effect: "The first player shuffles his deck and reveals the top card. He may
discard N cards from his hand that match the revealed card's ..." Each one printed in the
box reads so, only the count and what must match differing.
-}
data Burgle = Burgle {burgleCount :: Int, burgleMatching :: [BurgleMatch]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The cards printed with a burgle effect, and theirs.
burgleEffects :: [(CardCode, Burgle)]
burgleEffects =
  [ (toCardCode Enemies.croakingCrows, Burgle 1 [BySphere, ByType])
  , (toCardCode Locations.greatHall, Burgle 3 [ByType])
  , (toCardCode Locations.desolationOfSmaug, Burgle 2 [ByCost])
  , (toCardCode Locations.westernSlopes, Burgle 2 [BySphere])
  , (toCardCode Treacheries.pretendingToSleep, Burgle 2 [ByType])
  , (toCardCode Treacheries.knownToAnOunce, Burgle 1 [BySphere, ByCost])
  , (toCardCode Treacheries.belchingFire, Burgle 1 [ByType, ByCost])
  ]

-- | The burgle effect printed on the card, if it has one.
burgleOf :: HasCardCode a => a -> Maybe Burgle
burgleOf a = lookup (toCardCode a) burgleEffects

-- | A card printed with a burgle effect.
withBurgle :: CardMatcher
withBurgle = CardMatchAny $ map (CardWithCode . fst) burgleEffects

hasBurgle :: HasCardCode a => a -> Bool
hasBurgle = isJust . burgleOf

-- | Whether the card in hand matches the revealed card as the burgle effect asks: a
-- card's cost is its printed cost, an X counting 0.
burgleMatches :: Burgle -> Card -> Card -> Bool
burgleMatches b revealed card = all matching b.burgleMatching
 where
  r = toCardDef revealed
  c = toCardDef card
  matching = \case
    BySphere -> cdSphere r == cdSphere c
    ByType -> cdCardType r == cdCardType c
    ByCost -> printedCost r == printedCost c
  printedCost d = case cdCost d of
    Just (StaticCost n) -> n
    _ -> 0

-- | The question the first player is asked once his card is revealed, by what must match.
burgleQuestionKey :: Burgle -> Text
burgleQuestionKey b =
  "theLonelyMountain.burgle." <> case sort (map rank b.burgleMatching) of
    [0, 1] -> "sphereAndType"
    [0, 2] -> "sphereAndCost"
    [1, 2] -> "typeAndCost"
    [0] -> "sphere"
    [1] -> "type"
    _ -> "cost"
 where
  rank :: BurgleMatch -> Int
  rank = \case
    BySphere -> 0
    ByType -> 1
    ByCost -> 2

{- | The first player makes a burgle attempt for the card: what follows a success and a
failure is the card's to say, done whether or not the card is still in play by then. Any
card may read the outcome back with 'burgled', as stage 2B's Forced effect does.
-}
makeBurgleAttempt
  :: (ReverseQueue m, Targetable a) => a -> QueueT Message m () -> QueueT Message m () -> m ()
makeBurgleAttempt a onSuccess onFail = do
  succeeded <- capture onSuccess
  failed <- capture onFail
  lead <- getLead
  pushAll =<< inLogBlock (BurgleBlock lead) [ScenarioSpecific "burgle.attempt" (toJSON (toTarget a, succeeded, failed))]

-- | The outcome of a burgle attempt: the card it was made for, and whether it succeeded.
burgled :: Message -> Maybe (Target, Bool)
burgled = \case
  ScenarioSpecific "burgle.result" v -> parseMaybe parseJSON v
  _ -> Nothing

-- | The five Erebor treasures, shuffled and stacked facedown under The Lonely Mountain.
lonelyMountainTreasures :: [CardDef]
lonelyMountainTreasures =
  [ Attachments.theArkenstoneTreasure
  , Attachments.mithrilShirtTreasure
  , Attachments.throrsBattleAxe
  , Attachments.throrsHuntingBow
  , Attachments.throrsGoldenCup
  ]

getSmaugTheGolden :: HasGame m => m (Maybe EnemyId)
getSmaugTheGolden = headMay <$> select (EnemyWithCardCode $ toCardCode Enemies.smaugTheGolden)

getSmaugTheMagnificent :: HasGame m => m (Maybe EnemyId)
getSmaugTheMagnificent = headMay <$> select (EnemyWithCardCode $ toCardCode Enemies.smaugTheMagnificent)

getTheLonelyMountain :: HasGame m => m (Maybe LocationId)
getTheLonelyMountain = headMay <$> select (LocationWithCardCode $ toCardCode Locations.theLonelyMountain)

-- | The progress on the enemy, as on Smaug the Golden.
enemyProgress :: HasGame m => EnemyId -> m Int
enemyProgress eid = countTokens Progress . enemyTokens . toAttrs <$> getEnemy eid

-- | "Put 1 progress on Smaug the Golden": none while he is out of play.
placeProgressOnSmaugTheGolden :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeProgressOnSmaugTheGolden source n = traverse_ (\sg -> placeProgress source sg n) =<< getSmaugTheGolden

{- | "Advance to stage 4A": stage 4 is taken from the quest deck, the stage in play leaving
it, and stage 3 stays in the quest deck below for stage 4B to reset to. Nothing happens
once stage 4 is no longer in the quest deck.
-}
advanceToStageFour :: ReverseQueue m => m ()
advanceToStageFour = do
  deck <- getsGame (scenarioQuestDeck . toAttrs . (.scenario))
  for_ (find ((== toCardCode Quests.thievesFireMurder) . toCardCode) deck) \stage -> do
    mains <- getMainQuests
    for_ [qid | QuestTarget qid <- mains] (push . LeaveQuestStage)
    advanceToQuest stage
