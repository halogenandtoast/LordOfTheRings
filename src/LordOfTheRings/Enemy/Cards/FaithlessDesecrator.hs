module LordOfTheRings.Enemy.Cards.FaithlessDesecrator (faithlessDesecrator) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Ruins))

newtype FaithlessDesecrator = FaithlessDesecrator EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

faithlessDesecrator :: EnemyCard FaithlessDesecrator
faithlessDesecrator = enemy FaithlessDesecrator Cards.faithlessDesecrator

-- | While engaged with a player, Faithless Desecrator contributes its threat to the total
-- threat in the staging area.
instance HasModifiersFor FaithlessDesecrator where
  getModifiersFor (FaithlessDesecrator a) = modified a GameTarget [EngagedEnemiesAddThreat (EnemyWithId a.id)]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if the active location is a
-- Ruins location).
instance RunMessage FaithlessDesecrator where
  runMessage msg e@(FaithlessDesecrator attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ruins <- selectAny (IsActiveLocation <> LocationWithTrait Ruins)
      enemyGetsAttack (CardIdSource cid) eid (if ruins then 2 else 1)
      pure e
    _ -> FaithlessDesecrator <$> runMessage msg attrs
