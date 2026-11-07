module LordOfTheRings.Helpers.Character (module LordOfTheRings.Helpers.Character) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card hiding (enemy)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Entities (entitiesHeroes, entitiesObjectives)
import LordOfTheRings.Game.Base (Separation (separationShared), gameEntities, gameScenario, gameSeparation)
import LordOfTheRings.Message (EscapeStat (..), EscapeTest (..), FortitudeTest (..), HideTest (..))
import LordOfTheRings.Scenario.Types (scenarioEncounterDeck)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Helpers.Modifiers
import LordOfTheRings.Id
import LordOfTheRings.Keyword (Keyword (Battle, Siege, Spectral))
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Target

-- | The card definition behind a hero or ally target.
characterCardDef :: (HasCallStack, HasGame m) => Target -> m CardDef
characterCardDef = \case
  HeroTarget hid -> toCardDef <$> getHero hid
  AllyTarget aid -> toCardDef <$> getAlly aid
  ObjectiveTarget oid -> toCardDef <$> getObjective oid
  target -> error $ "not a character: " <> show target

characterStat :: (HasGame m) => (CardDef -> Maybe Int) -> (ModifierType -> Maybe Int) -> Target -> m Int
characterStat printed mods target = do
  def <- characterCardDef target
  modifiedValue mods target (fromMaybe 0 $ printed def)

getWillpower, getAttack, getDefense, getHitPoints :: (HasGame m) => Target -> m Int
getWillpower target = getWillpowerWith target []

-- | Combine temporary test bonuses with all other modifiers before flooring at zero.
-- A character that cannot have its willpower reduced ignores each negative modifier.
getWillpowerWith :: HasGame m => Target -> [Int] -> m Int
getWillpowerWith target bonuses = do
  mods <- getModifiers target
  def <- characterCardDef target
  pure $ willpowerValue def mods bonuses

getAttack target = do
  blank <- elem DoesNotCountAttack <$> getModifiers target
  if blank then pure 0 else characterStat cdAttack attackOf target
getDefense target = do
  blank <- elem DoesNotCountDefense <$> getModifiers target
  willpower <- elem WillpowerForDefense <$> ((<>) <$> getModifiers GameTarget <*> getModifiers target)
  if
    | blank -> pure 0
    | willpower -> getWillpower target
    | otherwise -> characterStat cdDefense defenseOf target
getHitPoints = characterStat cdHitPoints hitPointsOf

-- | The defense a character defends against the enemy with, with its bonuses against the
-- enemy's traits, as Orders from Angmar's against Undead.
getDefenseAgainst :: (HasGame m) => EnemyId -> Target -> m Int
getDefenseAgainst eid defender = do
  mods <- getModifiers defender
  global <- getModifiers GameTarget
  enemy <- toCardDef <$> getEnemy eid
  useWillpower <- anyM (\trait -> hasTrait trait (toTarget eid) enemy) [trait | WillpowerForDefenseAgainst trait <- mods <> global]
  if useWillpower then getWillpower defender
  else do
    defense <- getDefense defender
    bonus <- sum <$> for [(trait, n) | DefenseBonusAgainst trait n <- mods] \(trait, n) -> bool 0 n <$> hasTrait trait (toTarget eid) enemy
    byMatcher <- sum <$> for [(m, n) | DefenseBonusAgainstMatching m n <- mods] \(m, n) -> bool 0 n <$> selectAny (EnemyWithId eid <> m)
    -- none of the defense counts against an enemy that ignores that stat
    uncounted <- elem DefendersDoNotCountDefense <$> getModifiers (EnemyTarget eid)
    pure $ if uncounted then 0 else defense + bonus + byMatcher

-- | The attack the characters given make together against an enemy, with their bonuses
-- against its traits.
getAttackStrength :: (HasGame m) => [Target] -> EnemyId -> CardDef -> m Int
getAttackStrength attackers eid enemy = do
  bonuses <- for attackers \a -> do
    mods <- getModifiers a
    byTrait <- sum <$> for [(trait, n) | AttackBonusAgainst trait n <- mods] \(trait, n) -> bool 0 n <$> hasTrait trait (toTarget eid) enemy
    byMatcher <- sum <$> for [(m, n) | AttackBonusAgainstMatching m n <- mods] \(m, n) -> bool 0 n <$> selectAny (EnemyWithId eid <> m)
    pure $ byTrait + byMatcher
  willpower <- elem WillpowerForAttack <$> getModifiers GameTarget
  let strength a = do
        blank <- elem DoesNotCountAttack <$> getModifiers a
        if blank then pure 0 else getWillpower a
  spectral <- hasKeyword Spectral (EnemyTarget eid) enemy
  -- Spectral replaces the attackers' attack stat, including attack bonuses. The
  -- rule sheet leaves the enemy's defense and direct damage unchanged.
  if spectral
    then sum <$> traverse getWillpower attackers
    else (+ sum bonuses) . sum <$> traverse (if willpower then strength else getAttack) attackers

-- | The stat characters quest with, by name: attack under battle, defense under siege,
-- else willpower.
questingStat :: (HasGame m) => m (Text, Target -> m Int)
questingStat = do
  battle <- currentQuestHas Battle
  siege <- currentQuestHas Siege
  -- a quest with both, as East Quarter's battle on a siege quest, uses siege: the FAQ's
  -- errata settle it so each time, Trained for War's "if the current quest does not have
  -- siege, it gains battle" and The Cross-roads' "gains siege (and loses battle)"
  pure
    $ if
      | siege -> ("defense", getDefense)
      | battle -> ("attack", getAttack)
      | otherwise -> ("willpower", getWillpower)

-- | The total strength every character committed to the quest quests with.
getCommittedWillpower :: (HasGame m) => m Int
getCommittedWillpower = do
  (_, strength) <- questingStat
  -- one questing with its attack, as Grappling Hook's, does so whatever the quest's keyword
  let strength' c = do
        attack <- elem QuestsWithAttack <$> getModifiers c
        if attack then getAttack c else strength c
  sum
    <$> ( traverse strength'
            =<< filterM (fmap (notElem DoesNotCountWillpower) . getModifiers)
            =<< getCommittedCharacters
        )

-- | The characters committed to the quest, objective allies the staging area commits too.
getCommittedCharacters :: (HasGame m) => m [Target]
getCommittedCharacters = do
  -- sharing the game split between staging areas, those of the players at the stage
  shared <- getsGame (maybe False separationShared . gameSeparation)
  characters <-
    if shared
      then selectCharacters (CommittedHero <> HeroControlledBy PlayerAtThisStage) (CommittedAlly <> AllyControlledBy PlayerAtThisStage)
      else selectCharacters CommittedHero CommittedAlly
  objectives <- getsGame (map toTarget . toList . entitiesObjectives . gameEntities)
  (characters <>) <$> filterM (fmap (elem CountsAsCommitted) . getModifiers) objectives

{- | A Fortitude test's committed willpower, and the Eye icons on the cards discarded for it,
those whose icons are ignored left out, with any added to the test, as by Brake of Thorns.
-}
fortitudeTestTotals :: (HasGame m) => FortitudeTest -> m (Int, Int)
fortitudeTestTotals t = do
  heroes <- getsGame (entitiesHeroes . gameEntities)
  willpower <- sum <$> traverse (getWillpower . HeroTarget) (filter (`Map.member` heroes) t.fortitudeCommitted)
  mods <- getModifiers GameTarget
  let printed = sum [cdEyeIcons (toCardDef c) | c <- t.fortitudeDiscarded, c.id `notElem` t.fortitudeIgnored]
  pure (willpower, printed + t.fortitudeEyes + sum [n | FortitudeTestEyes n <- mods])

-- | An escape test's committed total, and the escape value of the cards it would deal
-- from the encounter deck as it stands.
escapeTestTotals :: (HasGame m) => EscapeTest -> m (Int, Int)
escapeTestTotals t = do
  total <- sum <$> traverse stat t.escapeCommitted
  deck <- getsGame (scenarioEncounterDeck . toAttrs . gameScenario)
  mods <- getModifiers GameTarget
  let bonus = sum [n | EscapeBonus n <- mods]
      -- added once to the test, as Stagnant Quagmire adds its Escape value
      testBonus = sum [n | EscapeTestBonus n <- mods]
  pure (total, testBonus + sum [fromMaybe 0 (cdEscape $ toCardDef c) + bonus | c <- take t.escapeCards deck])
 where
  stat = case t.escapeStat of
    EscapeWillpower -> getWillpower
    EscapeAttack -> getAttack
    EscapeDefense -> getDefense

-- | A Hide test's committed willpower, and the threat of the cards it discarded: treachery
-- cards have none (The Black Riders rules insert, "Hide Tests").
hideTestTotals :: (HasGame m) => HideTest -> m (Int, Int)
hideTestTotals t = do
  gameMods <- getModifiers GameTarget
  playerMods <- getModifiers t.hidePlayer
  let bonuses = [n | HideTestWillpower n <- gameMods <> playerMods]
  willpower <- sum <$> traverse (`getWillpowerWith` bonuses) t.hideCommitted
  let bonus = sum [n | HideTestThreat n <- gameMods <> playerMods]
  pure
    ( willpower
    , bonus + sum [fromMaybe 0 (cdThreat $ toCardDef c) | c <- t.hideDiscarded]
    )
