module LordOfTheRings.Enemy.Cards.OrcOfMordor (orcOfMordor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype OrcOfMordor = OrcOfMordor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge.
orcOfMordor :: EnemyCard OrcOfMordor
orcOfMordor = enemy OrcOfMordor Cards.orcOfMordor

-- | Forced: After Orc of Mordor engages you, exhaust a character you control.
instance HasAbilities OrcOfMordor where
  getAbilities (OrcOfMordor a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Exhaust a character you control.
instance RunMessage OrcOfMordor where
  runMessage msg e@(OrcOfMordor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> chooseAndExhaust attrs pid 1
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust (CardIdSource cid) pid 1
      pure e
    _ -> OrcOfMordor <$> runMessage msg attrs
