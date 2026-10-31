module LordOfTheRings.Enemy.Cards.Revenant (revenant) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Campaign (campaignCardResources)

newtype Revenant = Revenant EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Dream-chaser campaign. Forced: After Revenant is destroyed, shuffle it
-- into the encounter deck. Place 1 resource on the campaign card: the campaign card does
-- that, as it sees Revenant destroyed.
revenant :: EnemyCard Revenant
revenant = enemy Revenant Cards.revenant

-- | Immune to player card effects. Revenant gets +2 attack and +2 defense for each resource
-- on the campaign card.
-- Forced: When 3 or more damage would be placed on Revenant, place 2 damage instead.
instance HasModifiersFor Revenant where
  getModifiersFor (Revenant a) = do
    n <- campaignCardResources
    modifySelf a $ [ImmuneToPlayerCardEffects, DamageReducedTo 2, ShuffledIntoEncounterDeckWhenDefeated] <> [m | n > 0, m <- [AttackModifier (2 * n), DefenseModifier (2 * n)]]
