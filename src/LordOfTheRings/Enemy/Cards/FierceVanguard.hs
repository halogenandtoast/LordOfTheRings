module LordOfTheRings.Enemy.Cards.FierceVanguard (fierceVanguard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland.Helpers (assaultProgressLabeled)

newtype FierceVanguard = FierceVanguard EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fierceVanguard :: EnemyCard FierceVanguard
fierceVanguard = enemy FierceVanguard Cards.fierceVanguard

-- | Forced: After Fierce Vanguard engages you, either it gets +2 attack until the end of the
-- round, or place 1 progress on The Goblins' Assault.
instance HasAbilities FierceVanguard where
  getAbilities (FierceVanguard a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy gets +2 attack and +2 defense until the end of the phase.
instance RunMessage FierceVanguard where
  runMessage msg e@(FierceVanguard attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyEngaged _ pid) <- ws]) \pid ->
        chooseOneM pid do
          questionLabeled "fierceVanguard.choose"
          causedBy attrs
          labeled "fierceVanguard.attack" $ roundModifier attrs attrs.id (AttackModifier 2)
          assaultProgressLabeled attrs
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      phaseModifiers attrs eid [AttackModifier 2, DefenseModifier 2]
      pure e
    _ -> FierceVanguard <$> runMessage msg attrs
