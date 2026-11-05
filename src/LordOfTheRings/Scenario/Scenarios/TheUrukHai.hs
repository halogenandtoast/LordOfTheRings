module LordOfTheRings.Scenario.Scenarios.TheUrukHai (theUrukHai) where

import Data.Aeson.Types (parseMaybe)
import Data.List (elemIndex)
import LordOfTheRings.Campaign (CampaignLog (..), CampaignLogUpdate (..), PoolCard (..), updateLog)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Hero.Types (Hero)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher (getHero, getLead, getRemainingPlayers)
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Trait (Trait (Healer, Noble, Ranger, Warrior))
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (pursuitOf)

newtype TheUrukHai = TheUrukHai ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theUrukHai :: Scenario
theUrukHai = Scenario $ scenario TheUrukHai "the-uruk-hai" "The Uruk-hai"

{- | The Uruk-hai, Orcs of the White Hand and Snaga Orcs make up the encounter deck (MEC45
rules). Uglúk, To Isengard! and a copy of Eastemnet are set aside for stage 1A's setup.

The pursuit tracker is the scenario's: its value is kept in the meta, set to 10 as stage 1A
reads, and moved by 'increasePursuit', never below 0. Each stage's quest points are the
pursuit value, so each move has the stage checked again, as one with as much progress as
its quest points is defeated. Whether the Uruk-hai reach Isengard To Isengard! checks at
the end of the round, as its Forced effect reads.
-}
instance RunMessage TheUrukHai where
  runMessage msg (TheUrukHai attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheUrukHai, EncounterSet.OrcsOfTheWhiteHand, EncounterSet.SnagaOrcs]
      let aside = take 1 (filter (isCard Enemies.ugluk) cards) <> take 1 (filter (isCard Objectives.toIsengard) cards) <> take 1 (filter (isCard Locations.eastemnet) cards)
          deck = filter (`notElem` aside) cards
      firstStage <- genEncounterCard Quests.followTheOrcs
      secondStage <- genEncounterCard Quests.rescueTheCaptives
      putQuestIntoPlay firstStage
      pure
        $ TheUrukHai
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ aside
        & questDeckL .~ [secondStage]
        & metaL .~ object ["pursuit" .= (0 :: Int)]
        -- Campaign: Setup: Remove Gildor Inglorion and Mr. Underhill from the Campaign
        -- Pool, before its cards are dealt in. (Each burden of the Shadow of the Past,
        -- Flight to the Ford and The Ring Goes South burden sets is left out of the
        -- encounter deck: see 'removedBurdenSets'.)
        & campaignL %~ fmap (updateLog (RemoveFromPool gildorInglorion) . updateLog (RemoveFromPool mrUnderhill))
    ScenarioSpecific "urukHai.pursuit" (parseMaybe parseJSON -> Just (n :: Int)) -> do
      let p = max 0 (pursuitOf attrs.meta + n)
      checkStages
      pure $ TheUrukHai $ attrs & metaL .~ object ["pursuit" .= p]
    ScenarioSpecific "urukHai.setPursuit" (parseMaybe parseJSON -> Just (n :: Int)) -> do
      checkStages
      pure $ TheUrukHai $ attrs & metaL .~ object ["pursuit" .= max 0 n]
    AdvanceQuest | null attrs.questDeck, Just l <- attrs.campaign, pursuitOf attrs.meta <= 25 -> do
      resolution l
      TheUrukHai <$> runMessage msg attrs
    _ -> TheUrukHai <$> runMessage msg attrs
   where
    isCard def = (== toCardCode def) . toCardCode
    -- The Black Riders' boons, Gildor Inglorion the objective ally and Mr. Underhill
    gildorInglorion = "141077"
    mrUnderhill = "141017"
    checkStages = traverse_ (checkStage . toId) =<< getsGame (toList . (.entities.quests))

{- | The campaign card's resolution: If the pursuit value is 25 or lower, the players must
choose: either each player earns one of the Skill boons (Intimidation, Hands of a Healer,
Forewarned, or Leader of Men) and attaches it to a hero he controls, or one player earns
the boon, Beyond All Hope, and chooses a hero from the list of fallen heroes. That player
takes control of the chosen hero (removing it from the list of fallen heroes) and attaches
Beyond All Hope to it. If a player controls more than 3 non-Fellowship heroes, he must
remove heroes from the Campaign Log until he controls only 3 heroes. Changing heroes as a
result of earning Beyond All Hope does not incur a +1 threat penalty. Add the earned cards
to the Campaign Pool.

The choices are made as the last stage is defeated and the quest deck runs out, the
pursuit value then final: once the game is over nothing more can be asked, and the hero Beyond All Hope brings back must be on the
log before the next scenario's decks are chosen. What is earned is written to the log,
kept as the game is won; a Permanent boon is attached as its hero comes into play.

A player's heroes are the non-Fellowship heroes he owns still in play, his Captive among
them: the heroes the log records for him (the Fellowship hero is not recorded). Each Skill
boon goes to a hero its own Setup allows ("Attach to a Warrior hero" and so on), one copy
of each, so each player earns a different one.
-}
resolution :: ReverseQueue m => CampaignLog -> m ()
resolution l = do
  lead <- getLead
  fallen <- restorable
  chooseOneM lead do
    questionLabeled "theUrukHai.resolution"
    labeled "theUrukHai.skillBoons" $ eachEarnsASkill skillBoons =<< getRemainingPlayers
    when (notNull fallen) $ labeled "theUrukHai.beyondAllHope" do
      choosePlayerM lead ScenarioTarget \pid -> beyondAllHope pid fallen
 where
  -- Any fallen hero, save one sharing a title with a hero in play (the uniqueness rule).
  restorable = do
    heroes <- getsGame (toList . (.entities.heroes))
    let taken = map (title . toCardCode) heroes
    pure [c | c <- l.fallenHeroes, title c `notElem` taken]

-- | Intimidation, Hands of a Healer, Forewarned and Leader of Men, each with the trait its
-- Setup asks of its hero.
skillBoons :: [(Text, CardCode, Trait)]
skillBoons =
  [ ("intimidation", "143015", Warrior)
  , ("handsOfAHealer", "143016", Healer)
  , ("forewarned", "143017", Ranger)
  , ("leaderOfMen", "143018", Noble)
  ]

eachEarnsASkill :: ReverseQueue m => [(Text, CardCode, Trait)] -> [PlayerId] -> m ()
eachEarnsASkill _ [] = pure ()
eachEarnsASkill remaining (p : rest) = do
  heroes <- heroesOf p
  let eligible = [(key, code, hs) | (key, code, trait) <- remaining, let hs = [h | h <- heroes, trait `member` cdCardTraits (toCardDef h)], notNull hs]
  if null eligible
    then eachEarnsASkill remaining rest
    else chooseOneM p do
      questionLabeled "theUrukHai.chooseSkill"
      for_ eligible \(key, code, hs) -> labeled ("theUrukHai." <> key) do
        chooseOrRunOneM p do
          questionLabeled "theUrukHai.attachTo"
          targets (map (.id) hs) \h -> do
            h' <- getHero h
            updateCampaignLog $ AddToPool $ PoolCard code Nothing (Just $ toCardCode h')
        eachEarnsASkill [b | b@(_, c, _) <- remaining, c /= code] rest

-- | The player brings a fallen hero back with Beyond All Hope, giving up one of his own if
-- he would have more than 3.
beyondAllHope :: ReverseQueue m => PlayerId -> [CardCode] -> m ()
beyondAllHope pid fallen = do
  seat <- elemIndex pid <$> getsGame (.playerOrder)
  heroes <- heroesOf pid
  chooseOneM pid do
    questionLabeled "theUrukHai.chooseFallen"
    for_ fallen \code -> labeledWith "theUrukHai.fallenHero" (cardVar "hero" code) do
      updateCampaignLog $ RemoveFallenHero code
      updateCampaignLog $ AddToPool $ PoolCard beyondAllHopeCode Nothing (Just code)
      for_ seat \s ->
        if length heroes + 1 > 3
          then chooseOneM pid do
            questionLabeled "theUrukHai.removeHero"
            targets (map (.id) heroes) \h -> do
              h' <- getHero h
              updateCampaignLog $ ReplaceSeatHero s (Just $ toCardCode h') code
          else updateCampaignLog $ ReplaceSeatHero s Nothing code
 where
  beyondAllHopeCode = "143019"

-- | The non-Fellowship heroes the player owns in play, his Captive among them.
heroesOf :: HasGame m => PlayerId -> m [Hero]
heroesOf pid = do
  heroes <- getsGame (toList . (.entities.heroes))
  pure [h | h <- heroes, h.owner == pid, h.sphere /= Fellowship]

title :: CardCode -> Text
title code = let Name t _ = cdName (toCardDef code) in t
