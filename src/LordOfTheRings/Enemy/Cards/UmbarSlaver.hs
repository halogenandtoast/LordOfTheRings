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
-- resources on Umbar Slaver: once for each payment, however many heroes it came from.
instance RunMessage UmbarSlaver where
  runMessage msg (UmbarSlaver attrs) = case msg of
    CheckWindows ws | EngagedWith pid <- attrs.placement -> do
      for_ (take 1 [() | Window After (ResourcesSpent p _) <- ws, p == pid]) \_ -> placeResources attrs attrs 1
      UmbarSlaver <$> runMessage msg attrs
    _ -> UmbarSlaver <$> runMessage msg attrs
