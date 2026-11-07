module LordOfTheRings.Helpers.Healing (healingAmount, healableCharacters) where

import LordOfTheRings.Card (lookupCardDef)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Log.Refs (cardTitle, sourceCardTitle)
import LordOfTheRings.Matcher (getDamage, damagedCharacters)
import LordOfTheRings.Matcher.Types (HeroMatcher, AllyMatcher)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Source (Source)
import LordOfTheRings.Target (Target, Targetable (toTarget))

-- The same limits govern legal healing targets and the amount ultimately healed.
healingAmount :: (HasGame m, Targetable target) => Source -> target -> Int -> m Int
healingAmount source target amount = do
  damage <- getDamage (toTarget target)
  mods <- getModifiers target
  g <- getGame
  -- Rules Reference: a card named by title includes every printing of that title.
  let from = sourceCardTitle g source
      title code = cardTitle <$> lookupCardDef code
      healedThisRound = sum [n | HealedThisRound n <- mods]
      limits = [n | HealLimit n <- mods]
        <> [n | HealLimitExceptFrom n code <- mods, from /= title code]
        <> [max 0 (n - healedThisRound) | HealLimitEachRound n <- mods]
  pure $ if CannotBeHealed `elem` mods then 0 else max 0 $ foldr min (min amount damage) limits

healableCharacters :: HasGame m => Source -> HeroMatcher -> AllyMatcher -> m [Target]
healableCharacters source heroes allies =
  filterM (\target -> (> 0) <$> healingAmount source target 1) =<< damagedCharacters heroes allies
