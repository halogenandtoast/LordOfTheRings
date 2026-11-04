module LordOfTheRings.Enemy.Cards.OrcOfTheFlame (orcOfTheFlame) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FireInTheNight.Helpers (damageHrogarsHill, getHrogarsHill)

newtype OrcOfTheFlame = OrcOfTheFlame EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcOfTheFlame :: EnemyCard OrcOfTheFlame
orcOfTheFlame = enemy OrcOfTheFlame Cards.orcOfTheFlame

{- | When Revealed: The first player must either deal 2 damage to a hero he controls, or
deal 3 damage to Hrogar's Hill.
Shadow: Attacking enemy gets +1 [attack] (+2 [attack] instead if Hrogar's Hill has at
least 25 damage on it).
-}
instance RunMessage OrcOfTheFlame where
  runMessage msg e@(OrcOfTheFlame attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "orcOfTheFlame.choose"
        causedBy attrs
        labeled "orcOfTheFlame.hero" $ dealDamageToOneHero attrs lead 2
        labeled "orcOfTheFlame.hill" $ damageHrogarsHill attrs 3
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      damage <- maybe (pure 0) (getDamage . QuestTarget) =<< getHrogarsHill
      enemyGetsAttack attrs eid (if damage >= 25 then 2 else 1)
      pure e
    _ -> OrcOfTheFlame <$> runMessage msg attrs
