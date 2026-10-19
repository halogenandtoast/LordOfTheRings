module LordOfTheRings.Ally.Cards.GondorianSpearman (gondorianSpearman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype GondorianSpearman = GondorianSpearman AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gondorianSpearman :: AllyCard GondorianSpearman
gondorianSpearman = ally GondorianSpearman Cards.gondorianSpearman

-- | Sentinel. Response: After Gondorian Spearman is declared as a defender, deal 1 damage
-- to the attacking enemy.
instance HasAbilities GondorianSpearman where
  getAbilities (GondorianSpearman a) = [mkAbility a 1 $ response $ DefenderDeclaredWindow After (Just $ toTarget a)]

instance RunMessage GondorianSpearman where
  runMessage msg s@(GondorianSpearman a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      pushAll $ take 1 [DealDamage (toSource a) (toTarget eid) 1 | Window _ (DeclaredAsDefender _ eid) <- windows]
      pure s
    _ -> GondorianSpearman <$> runMessage msg a
