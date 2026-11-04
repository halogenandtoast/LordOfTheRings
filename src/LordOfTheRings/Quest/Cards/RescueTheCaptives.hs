module LordOfTheRings.Quest.Cards.RescueTheCaptives (rescueTheCaptives) where

import Data.List (elemIndex)
import LordOfTheRings.Campaign (CampaignLog (..), CampaignLogUpdate (..), PoolCard (..))
import LordOfTheRings.Card (CardType (EnemyType), cdCardTraits, cdName, toCardCode, toCardDef)
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Hero.Types (Hero)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Saga (sagaBarredTitles)
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (getPursuit, reducePursuit)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioCampaign))
import LordOfTheRings.Sphere (Sphere (Fellowship))
import LordOfTheRings.Trait (Trait (Healer, Noble, Ranger, UrukHai, Warrior))

newtype RescueTheCaptives = RescueTheCaptives QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

rescueTheCaptives :: QuestCard RescueTheCaptives
rescueTheCaptives = quest RescueTheCaptives Cards.rescueTheCaptives

-- | X is the pursuit value (1 printed, see 'Cards.rescueTheCaptives').
instance HasModifiersFor RescueTheCaptives where
  getModifiersFor (RescueTheCaptives a) = do
    pursuit <- getPursuit
    modifySelf a [QuestPointModifier (pursuit - 1)]

-- | Response: After an enemy is defeated, reduce the pursuit value by 1.
instance HasAbilities RescueTheCaptives where
  getAbilities (RescueTheCaptives a) = [mkAbility a 1 $ response $ EnemyDefeatedWindow After | a.flipped]

{- | When Revealed: Each player searches the encounter deck and discard pile for an Uruk-hai
enemy, reveals it, and adds it to the staging area. Shuffle the encounter deck. Each player
reveals his as its revealer, "you" on it reading him.

The players cannot defeat this stage while Uglúk is in play. If the players defeat this
stage, they have rescued each Captive hero and won the game.
-}
instance RunMessage RescueTheCaptives where
  runMessage msg q@(RescueTheCaptives attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      RescueTheCaptives <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> asRevealer p $ searchEncounterDeckAndDiscardToReveal p attrs (CardWithType EnemyType <> CardWithTrait UrukHai)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      reducePursuit 1
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      campaign <- getsGame (scenarioCampaign . toAttrs . (.scenario))
      pursuit <- getPursuit
      for_ campaign \l -> when (pursuit <= 25) $ resolution l
      winGame
      pure q
    _ -> RescueTheCaptives <$> runBlockedStage (EnemyWithCardCode $ toCardCode Enemies.ugluk) msg attrs

{- | The campaign card's resolution: If the pursuit value is 25 or lower, the players must
choose: either each player earns one of the Skill boons (Intimidation, Hands of a Healer,
Forewarned, or Leader of Men) and attaches it to a hero he controls, or one player earns
the boon, Beyond All Hope, and chooses a hero from the list of fallen heroes. That player
takes control of the chosen hero (removing it from the list of fallen heroes) and attaches
Beyond All Hope to it. If a player controls more than 3 non-Fellowship heroes, he must
remove heroes from the Campaign Log until he controls only 3 heroes. Changing heroes as a
result of earning Beyond All Hope does not incur a +1 threat penalty. Add the earned cards
to the Campaign Pool.

The choices are made as the stage is defeated, the pursuit value then final: once the game
is over nothing more can be asked, and the hero Beyond All Hope brings back must be on the
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
  -- the titles no player may use in Helm's Deep, and those of the heroes the players have
  restorable = do
    heroes <- getsGame (toList . (.entities.heroes))
    let taken = map (title . toCardCode) heroes
        barred = sagaBarredTitles "helms-deep" True
    pure [c | c <- l.fallenHeroes, title c `notElem` taken, title c `notElem` barred]

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
            hero <- getHero h
            updateCampaignLog $ AddToPool $ PoolCard code Nothing (Just $ toCardCode hero)
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
              hero <- getHero h
              updateCampaignLog $ ReplaceSeatHero s (Just $ toCardCode hero) code
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
