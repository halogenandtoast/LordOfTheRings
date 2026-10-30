module LordOfTheRings.Enemy.Cards.UmbarSlaver (umbarSlaver) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype UmbarSlaver = UmbarSlaver EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

umbarSlaver :: EnemyCard UmbarSlaver
umbarSlaver = enemy UmbarSlaver Cards.umbarSlaver

-- | Umbar Slaver gets +1 attack and +1 threat for each resource on it.
instance HasModifiersFor UmbarSlaver where
  getModifiersFor (UmbarSlaver a) = do
    let n = countTokens Resource a.enemyTokens
    when (n > 0) $ modifySelf a [AttackModifier n, ThreatModifier n]

-- | Forced: After the engaged player spends any number of resources, place 1 of those
-- resources on Umbar Slaver: as they leave a hero's pool.
-- ponytail: one resource for each hero paid from, not each payment; a "spent resources"
-- window would count a payment from two heroes once.
instance RunMessage UmbarSlaver where
  runMessage msg (UmbarSlaver attrs) = case msg of
    RemoveTokens (PlayerSource pid) (HeroTarget _) Resource n | n > 0, EngagedWith pid == attrs.placement -> do
      placeResources attrs attrs 1
      UmbarSlaver <$> runMessage msg attrs
    _ -> UmbarSlaver <$> runMessage msg attrs
