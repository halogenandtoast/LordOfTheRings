module LordOfTheRings.Enemy.Cards.OrcOfTheEye (orcOfTheEye) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (chooseDamageOrMinasTirith)
import LordOfTheRings.Trait (Trait (Orc))

newtype OrcOfTheEye = OrcOfTheEye EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril. Archery 1.
orcOfTheEye :: EnemyCard OrcOfTheEye
orcOfTheEye = enemy OrcOfTheEye Cards.orcOfTheEye

{- | When Revealed: Either deal 2 damage to a hero you control, or deal 2 damage to Minas
Tirith.
Shadow: Attacking enemy gets +1 attack (+2 attack instead if it is an Orc).
-}
instance RunMessage OrcOfTheEye where
  runMessage msg e@(OrcOfTheEye attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      chooseDamageOrMinasTirith attrs you "orcOfTheEye" 2 $ dealDamageToOneHero attrs you 2
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      orc <- selectAny (EnemyWithId eid <> EnemyWithTrait Orc)
      enemyGetsAttack attrs eid (if orc then 2 else 1)
      pure e
    _ -> OrcOfTheEye <$> runMessage msg attrs
