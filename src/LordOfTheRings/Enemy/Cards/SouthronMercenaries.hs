module LordOfTheRings.Enemy.Cards.SouthronMercenaries (southronMercenaries) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Trait (Trait (Harad))

newtype SouthronMercenaries = SouthronMercenaries EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronMercenaries :: EnemyCard SouthronMercenaries
southronMercenaries = enemy SouthronMercenaries Cards.southronMercenaries

-- | Archery X. X is the number of players in the game.
instance HasModifiersFor SouthronMercenaries where
  getModifiersFor (SouthronMercenaries a) = do
    players <- length <$> getRemainingPlayers
    modified a a [GainKeyword (Archery players)]

-- | Shadow: attacking enemy gets +1 attack. (+3 attack instead if it has the Harad trait.)
instance RunMessage SouthronMercenaries where
  runMessage msg e@(SouthronMercenaries attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      harad <- selectAny (EnemyWithId eid <> EnemyWithTrait Harad)
      enemyGetsAttack attrs eid (if harad then 3 else 1)
      pure e
    _ -> SouthronMercenaries <$> runMessage msg attrs
