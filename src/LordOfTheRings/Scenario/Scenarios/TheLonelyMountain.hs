{- | The Lonely Mountain. Stage 1A's setup takes Smaug the Golden, Smaug the Magnificent,
The Lonely Mountain and A Bare Patch from those set aside as the encounter deck is
gathered. The scenario makes each burgle attempt and keeps the treasures the players take
from under The Lonely Mountain, which they discover by winning (The Hobbit: On the
Doorstep rules insert, "Burgle" and "Treasure").
-}
module LordOfTheRings.Scenario.Scenarios.TheLonelyMountain (theLonelyMountain) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Campaign (PoolCard (..), addToPool)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (seatOf)
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers
import LordOfTheRings.Source (Source (ScenarioSource))

newtype TheLonelyMountain = TheLonelyMountain ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLonelyMountain :: Scenario
theLonelyMountain = Scenario $ scenario TheLonelyMountain "the-lonely-mountain" "The Lonely Mountain"

-- | A burgle attempt under way: the card it is made for and what follows its success and
-- its failure, the burgle effect discarded for it, the card revealed from the first
-- player's deck, and whether the encounter discard pile was already shuffled back in
-- looking for a burgle effect.
data Burgling = Burgling
  { burglingFor :: Target
  , burglingOnSuccess :: [Message]
  , burglingOnFail :: [Message]
  , burglingEffect :: Maybe Burgle
  , burglingRevealed :: Maybe Card
  , burglingReshuffled :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The attempt under way, and each treasure taken from under The Lonely Mountain with the
-- player who took it.
data Meta = Meta
  { metaBurgling :: Maybe Burgling
  , metaTaken :: [(CardCode, PlayerId)]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

metaOf :: ScenarioAttrs -> Meta
metaOf attrs = fromMaybe (Meta Nothing []) $ parseMaybe parseJSON attrs.scenarioMeta

{- | The Lonely Mountain and Wilderland make up the encounter deck (The Hobbit: On the
Doorstep rules insert). The first player takes control of Bilbo Baggins as the game sets
up (see 'LordOfTheRings.Saga.fellowshipSetup').
-}
instance RunMessage TheLonelyMountain where
  runMessage msg (TheLonelyMountain attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheLonelyMountain, EncounterSet.Wilderland]
      let searched = CardMatchAny [cardIs Enemies.smaugTheGolden, cardIs Enemies.smaugTheMagnificent, cardIs Locations.theLonelyMountain, cardIs Objectives.aBarePatch]
          (aside, deck) = partition (cardMatches searched) cards
      firstStage <- genEncounterCard Quests.aWarmWelcome
      laterStages <- traverse genEncounterCard [Quests.theBurglarsTurnTheLonelyMountain, Quests.insideInformation, Quests.thievesFireMurder]
      putQuestIntoPlay firstStage
      pure
        $ TheLonelyMountain
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ aside
        & questDeckL .~ laterStages
    -- Step 1: the first player discards cards from the top of the encounter deck until one
    -- with a burgle effect is discarded.
    ScenarioSpecific "burgle.attempt" (parseMaybe parseJSON -> Just (madeFor, onSuccess, onFail)) -> do
      push $ ScenarioSpecific "burgle.search" Null
      pure $ withMeta st {metaBurgling = Just $ Burgling madeFor onSuccess onFail Nothing Nothing False}
    ScenarioSpecific "burgle.search" _ -> do
      discardEncounterCardsUntilFor ScenarioSource ScenarioTarget withBurgle
      pure $ TheLonelyMountain attrs
    DiscardedTopOfEncounterDeck ScenarioSource ScenarioTarget discarded
      | Just b <- st.metaBurgling
      , isNothing b.burglingEffect -> case lastMay (filter hasBurgle discarded) >>= burgleOf of
          -- Step 2: he shuffles his deck and reveals the top card
          Just effect -> do
            lead <- getLead
            shufflePlayerDeck lead
            push $ ScenarioSpecific "burgle.reveal" Null
            pure $ withMeta st {metaBurgling = Just b {burglingEffect = Just effect}}
          -- none left in the deck: once the discard pile is shuffled back in, the search
          -- goes on
          Nothing | not b.burglingReshuffled -> do
            shuffleEncounterDiscardIntoDeck AnyCard
            push $ ScenarioSpecific "burgle.search" Null
            pure $ withMeta st {metaBurgling = Just b {burglingReshuffled = True}}
          Nothing -> burgleResult b False
    -- with no cards left in his deck, the burgle attempt automatically fails
    ScenarioSpecific "burgle.reveal" _ | Just b <- st.metaBurgling -> do
      lead <- getLead
      deck <- (.deck) <$> getPlayer lead
      case deck of
        top : _ -> do
          push $ ScenarioSpecific "burgle.revealed" (toJSON top)
          pure $ TheLonelyMountain attrs
        [] -> burgleResult b False
    -- Step 3: he may discard the cards the burgle effect asks for to succeed. While making
    -- the attempt, The Lonely Mountain's Action lets him spend 2 Baggins resources to
    -- take the revealed card into hand and reveal the next card of his deck.
    ScenarioSpecific "burgle.revealed" (parseMaybe parseJSON -> Just revealed)
      | Just b <- st.metaBurgling
      , Just effect <- b.burglingEffect -> do
          lead <- getLead
          hand <- (.hand) <$> getPlayer lead
          baggins <- getResourcesOfSphere lead Baggins
          mountain <- isJust <$> getTheLonelyMountain
          let matching = filter (burgleMatches effect revealed) hand
          chooseOneM lead do
            questionLabeledWith (burgleQuestionKey effect) [intVar "count" effect.burgleCount]
            showing [revealed]
            when (length matching >= effect.burgleCount)
              $ labeled "theLonelyMountain.burgle.discard"
              $ push
              $ ScenarioSpecific "burgle.discard" (toJSON effect.burgleCount)
            when (mountain && baggins >= 2) $ labeled "theLonelyMountain.burgle.takeRevealed" do
              spendResources lead Baggins 2
              takeFromDeck lead revealed
              push $ ScenarioSpecific "burgle.reveal" Null
            labeled "theLonelyMountain.burgle.fail" $ push $ ScenarioSpecific "burgle.fail" Null
          pure $ withMeta st {metaBurgling = Just b {burglingRevealed = Just revealed}}
    -- the matching cards are discarded one by one, the first player choosing which
    ScenarioSpecific "burgle.discard" (parseMaybe parseJSON -> Just (n :: Int))
      | Just b <- st.metaBurgling
      , Just effect <- b.burglingEffect
      , Just revealed <- b.burglingRevealed ->
          if n <= 0
            then burgleResult b True
            else do
              lead <- getLead
              hand <- (.hand) <$> getPlayer lead
              let matching = filter (burgleMatches effect revealed) hand
              if length matching < n
                then burgleResult b False
                else do
                  chooseOrRunOneM lead do
                    questionLabeledWith "theLonelyMountain.burgle.discardOne" [intVar "count" n]
                    showing [revealed]
                    for_ matching \card -> targeting (CardIdTarget card.id) do
                      discardFromHand lead card
                      push $ ScenarioSpecific "burgle.discard" (toJSON (n - 1))
                  pure $ TheLonelyMountain attrs
    ScenarioSpecific "burgle.fail" _ | Just b <- st.metaBurgling -> burgleResult b False
    -- a treasure taken from under The Lonely Mountain into a player's hand
    PutInHand pc | toCardCode pc `elem` map toCardCode lonelyMountainTreasures, Just owner <- pc.owner -> do
      let taken = st.metaTaken <> [(toCardCode pc, owner) | (toCardCode pc, owner) `notElem` st.metaTaken]
      TheLonelyMountain <$> runMessage msg (attrs & metaL .~ toJSON st {metaTaken = taken})
    -- Won, the players have discovered each treasure successfully burgled: the player who
    -- took it may add it to his deck in the scenarios that list its set (The Hobbit: On the
    -- Doorstep rules insert, "Treasure").
    ResolveCampaign | Just l <- attrs.campaign -> do
      found <- for st.metaTaken \(code, pid) -> fmap (code,) <$> seatOf pid
      let treasure (code, seat) = PoolCard {poolCardCode = code, poolCardSeat = Just seat, poolCardAttachedTo = Nothing}
          l' = foldl' (flip addToPool) l (map treasure $ catMaybes found)
      TheLonelyMountain <$> runMessage msg (attrs & campaignL ?~ l')
    _ -> TheLonelyMountain <$> runMessage msg attrs
   where
    st = metaOf attrs
    withMeta m = TheLonelyMountain $ attrs & metaL .~ toJSON m
    burgleResult b ok = do
      push $ ScenarioSpecific "burgle.result" (toJSON (b.burglingFor, ok))
      pushAll $ if ok then b.burglingOnSuccess else b.burglingOnFail
      pure $ withMeta st {metaBurgling = Nothing}
