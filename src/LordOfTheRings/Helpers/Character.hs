module LordOfTheRings.Helpers.Character (module LordOfTheRings.Helpers.Character) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Helpers.Modifiers
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Target

-- | The card definition behind a hero or ally target.
characterCardDef :: (HasCallStack, HasGame m) => Target -> m CardDef
characterCardDef = \case
  HeroTarget hid -> toCardDef <$> getHero hid
  AllyTarget aid -> toCardDef <$> getAlly aid
  target -> error $ "not a character: " <> show target

characterStat :: (HasGame m) => (CardDef -> Maybe Int) -> (ModifierType -> Maybe Int) -> Target -> m Int
characterStat printed mods target = do
  def <- characterCardDef target
  modifiedValue mods target (fromMaybe 0 $ printed def)

getWillpower, getAttack, getDefense, getHitPoints :: (HasGame m) => Target -> m Int
getWillpower = characterStat cdWillpower willpowerOf
getAttack = characterStat cdAttack attackOf
getDefense target = do
  blank <- elem DoesNotCountDefense <$> getModifiers target
  if blank then pure 0 else characterStat cdDefense defenseOf target
getHitPoints = characterStat cdHitPoints hitPointsOf

-- | The attack the characters given make together against an enemy, with their bonuses
-- against its traits.
getAttackStrength :: (HasGame m) => [Target] -> EnemyId -> CardDef -> m Int
getAttackStrength attackers eid enemy = do
  bonuses <- for attackers \a -> do
    mods <- getModifiers a
    sum <$> for [(trait, n) | AttackBonusAgainst trait n <- mods] \(trait, n) -> bool 0 n <$> hasTrait trait (toTarget eid) enemy
  (+ sum bonuses) . sum <$> traverse getAttack attackers

-- | The total willpower of every character committed to the quest.
getCommittedWillpower :: (HasGame m) => m Int
getCommittedWillpower = sum <$> (traverse getWillpower =<< selectCharacters CommittedHero CommittedAlly)
