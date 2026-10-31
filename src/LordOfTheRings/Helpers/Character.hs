module LordOfTheRings.Helpers.Character (module LordOfTheRings.Helpers.Character) where

import LordOfTheRings.Card hiding (enemy)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities, gameScenario)
import LordOfTheRings.Message (EscapeStat (..), EscapeTest (..))
import LordOfTheRings.Scenario.Types (scenarioEncounterDeck)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Helpers.Modifiers
import LordOfTheRings.Id
import LordOfTheRings.Keyword (Keyword (Battle, Siege))
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
getWillpower target = do
  -- one that cannot have its willpower reduced, as Star Brooch's hero, ignores what would
  floored <- elem CannotHaveWillpowerReduced <$> getModifiers target
  characterStat cdWillpower (if floored then (\m -> willpowerOf m >>= \n -> n <$ guard (n > 0)) else willpowerOf) target
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
  defense <- getDefense defender
  mods <- getModifiers defender
  enemy <- toCardDef <$> getEnemy eid
  bonus <- sum <$> for [(trait, n) | DefenseBonusAgainst trait n <- mods] \(trait, n) -> bool 0 n <$> hasTrait trait (toTarget eid) enemy
  byMatcher <- sum <$> for [(m, n) | DefenseBonusAgainstMatching m n <- mods] \(m, n) -> bool 0 n <$> selectAny (EnemyWithId eid <> m)
  pure $ defense + bonus + byMatcher

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
  (+ sum bonuses) . sum <$> traverse (if willpower then strength else getAttack) attackers

-- | The stat characters quest with, by name: attack under battle, defense under siege,
-- else willpower.
questingStat :: (HasGame m) => m (Text, Target -> m Int)
questingStat = do
  battle <- currentQuestHas Battle
  siege <- currentQuestHas Siege
  -- ponytail: no rule covers a quest with both battle and siege (the cards' errata, as The
  -- Cross-roads' "and loses battle", avoid it), so battle wins
  pure
    $ if
      | battle -> ("attack", getAttack)
      | siege -> ("defense", getDefense)
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
  characters <- selectCharacters CommittedHero CommittedAlly
  objectives <- getsGame (map toTarget . toList . entitiesObjectives . gameEntities)
  (characters <>) <$> filterM (fmap (elem CountsAsCommitted) . getModifiers) objectives

-- | An escape test's committed total, and the escape value of the cards it would deal
-- from the encounter deck as it stands.
escapeTestTotals :: (HasGame m) => EscapeTest -> m (Int, Int)
escapeTestTotals t = do
  total <- sum <$> traverse stat t.escapeCommitted
  deck <- getsGame (scenarioEncounterDeck . toAttrs . gameScenario)
  bonus <- sum . mapMaybe (\case EscapeBonus n -> Just n; _ -> Nothing) <$> getModifiers GameTarget
  pure (total, sum [fromMaybe 0 (cdEscape $ toCardDef c) + bonus | c <- take t.escapeCards deck])
 where
  stat = case t.escapeStat of
    EscapeWillpower -> getWillpower
    EscapeAttack -> getAttack
