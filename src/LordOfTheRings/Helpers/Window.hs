module LordOfTheRings.Helpers.Window (module LordOfTheRings.Helpers.Window) where

import LordOfTheRings.Ability
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities
import LordOfTheRings.Game.Base
import LordOfTheRings.Hero.Types
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window

getAllAbilities :: (HasGame m) => m [Ability]
getAllAbilities = do
  es <- getsGame gameEntities
  pure
    $ concat
      [ concatMap getAbilities (toList es.heroes)
      , concatMap getAbilities (toList es.allies)
      , concatMap getAbilities (toList es.enemies)
      , concatMap getAbilities (toList es.locations)
      , concatMap getAbilities (toList es.treacheries)
      , concatMap getAbilities (toList es.quests)
      ]

-- | Abilities whose window matches one of the windows and that can be used right now.
getWindowAbilities :: (HasGame m) => [Window] -> m [Ability]
getWindowAbilities windows = do
  abilities <- getAllAbilities
  flip filterM abilities \ab -> case abilityWindow ab of
    Just matcher | any (windowMatches matcher) windows -> andM [passesCriteria ab.abilityCriteria, canPay (abilityCost ab)]
    _ -> pure False

passesCriteria :: (HasGame m) => Criterion -> m Bool
passesCriteria = \case
  NoCriteria -> pure True
  LocationExists m -> selectAny m
  EnemyExists m -> selectAny m
  HeroExists m -> selectAny m
  AllyExists m -> selectAny m
  Criteria cs -> allM passesCriteria cs

canPay :: (HasGame m) => AbilityCost -> m Bool
canPay = \case
  Free -> pure True
  ExhaustCost (HeroTarget hid) -> selectAny (HeroWithId hid <> ReadyHero)
  ExhaustCost (AllyTarget aid) -> selectAny (AllyWithId aid <> ReadyAlly)
  ExhaustCost _ -> pure False
  ResourceCost (HeroTarget hid) n -> (>= n) . (.resources) <$> getHero hid
  ResourceCost _ _ -> pure False
  Costs cs -> allM canPay cs

costMessages :: Source -> AbilityCost -> [Message]
costMessages source = \case
  Free -> []
  ExhaustCost target -> [Exhaust target]
  ResourceCost target n -> [RemoveTokens source target Resource n]
  Costs cs -> concatMap (costMessages source) cs

-- | The player who controls an ability's source, if anyone does.
abilityController :: (HasGame m) => Ability -> m (Maybe PlayerId)
abilityController ab = go ab.abilitySource
 where
  go = \case
    AbilitySource s _ -> go s
    HeroSource hid -> Just . (.owner) <$> getHero hid
    AllySource aid -> placementController . (.placement) <$> getAlly aid
    PlayerSource pid -> pure $ Just pid
    _ -> pure Nothing

abilityKey :: Ability -> (Source, Int)
abilityKey ab = (ab.abilitySource, ab.abilityIndex)

-- | Players in turn order, starting with the first player.
getPlayerOrder :: (HasGame m) => m [PlayerId]
getPlayerOrder = do
  order <- getsGame gamePlayerOrder
  firstPlayer <- getsGame gameFirstPlayer
  let (before, after) = break (== firstPlayer) order
  pure $ after <> before

