module LordOfTheRings.Enemy.Cards.HuntersFromMordor (huntersFromMordor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Clue))

newtype HuntersFromMordor = HuntersFromMordor EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

huntersFromMordor :: EnemyCard HuntersFromMordor
huntersFromMordor = enemy HuntersFromMordor Cards.huntersFromMordor

-- | Hunters from Mordor get +2 threat and +2 attack for each Clue card in play.
instance HasModifiersFor HuntersFromMordor where
  getModifiersFor (HuntersFromMordor a) = do
    clues <- length <$> objectivesWithTrait Clue
    when (clues > 0) $ modified a a [ThreatModifier (2 * clues), AttackModifier (2 * clues)]

-- | Shadow: Deal 1 damage to each hero with a Clue card attached. (3 damage instead if this
-- attack is undefended.)
instance RunMessage HuntersFromMordor where
  runMessage msg e@(HuntersFromMordor attrs) = case msg of
    ResolveShadowEffect cid eid _ defender | cid == attrs.cardId -> do
      heroes <- select (HeroWithAttached Clue)
      for_ heroes \h -> dealDamage (EnemySource eid) h (ifUndefended defender 3 1)
      pure e
    _ -> HuntersFromMordor <$> runMessage msg attrs
