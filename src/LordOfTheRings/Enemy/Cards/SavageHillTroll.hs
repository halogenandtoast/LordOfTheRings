module LordOfTheRings.Enemy.Cards.SavageHillTroll (savageHillTroll) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Troll))

newtype SavageHillTroll = SavageHillTroll EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | While Savage Hill-troll is in the victory display, each Troll enemy gets +1 hit point.
-- (Conflict at the Carrock reads the victory display.)
savageHillTroll :: EnemyCard SavageHillTroll
savageHillTroll = enemy SavageHillTroll Cards.savageHillTroll

-- | Shadow: If attacking enemy is a Troll, defending character does not ready during the
-- refresh phase this round.
instance RunMessage SavageHillTroll where
  runMessage msg e@(SavageHillTroll attrs) = case msg of
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      troll <- selectAny (EnemyWithId eid <> EnemyWithTrait Troll)
      when troll $ for_ defenders \d -> roundModifier attrs d DoesNotReadyDuringRefresh
      pure e
    _ -> SavageHillTroll <$> runMessage msg attrs
