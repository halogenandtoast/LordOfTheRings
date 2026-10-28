module LordOfTheRings.Ally.Cards.IthilienArcher (ithilienArcher) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype IthilienArcher = IthilienArcher AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ithilienArcher :: AllyCard IthilienArcher
ithilienArcher = ally IthilienArcher Cards.ithilienArcher

-- | Response: After Ithilien Archer attacks and damages an enemy, return that enemy to the
-- staging area.
instance HasAbilities IthilienArcher where
  getAbilities (IthilienArcher a) = [mkAbility a 1 $ response $ AttackDealtDamageByWindow After (toTarget a)]

instance RunMessage IthilienArcher where
  runMessage msg s@(IthilienArcher a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      enemies <- select (not_ EnemyInStagingArea)
      for_ [eid | Window _ (AttackDealtDamage eid _ _) <- windows, eid `elem` enemies] \eid ->
        placeEnemy eid StagingArea
      pure s
    _ -> IthilienArcher <$> runMessage msg a
