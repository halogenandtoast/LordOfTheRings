module LordOfTheRings.Enemy.Cards.UrukOfMordor (urukOfMordor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Staging (getEnemyThreat)

newtype UrukOfMordor = UrukOfMordor EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukOfMordor :: EnemyCard UrukOfMordor
urukOfMordor = enemy UrukOfMordor Cards.urukOfMordor

-- | Each ally with less willpower than Uruk of Mordor's threat cannot defend against Uruk
-- of Mordor.
instance HasModifiersFor UrukOfMordor where
  getModifiersFor (UrukOfMordor a) = do
    threat <- getEnemyThreat a.id
    allies <- select AnyAlly
    for_ allies \al -> do
      willpower <- getWillpower (toTarget al)
      when (willpower < threat) $ modified a al [CannotDefendAgainst (EnemyWithId a.id)]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if its threat is greater than
-- the defending character's willpower). Undefended, there is no defending character, and
-- it gets +1.
instance RunMessage UrukOfMordor where
  runMessage msg e@(UrukOfMordor attrs) = case msg of
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      threat <- getEnemyThreat eid
      willpowers <- traverse getWillpower defenders
      enemyGetsAttack (CardIdSource cid) eid $ if any (< threat) willpowers then 2 else 1
      pure e
    _ -> UrukOfMordor <$> runMessage msg attrs
