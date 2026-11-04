module LordOfTheRings.Ally.Cards.LegolasTheTreasonOfSaruman (legolasTheTreasonOfSaruman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype LegolasTheTreasonOfSaruman = LegolasTheTreasonOfSaruman AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged, on its card def.
legolasTheTreasonOfSaruman :: AllyCard LegolasTheTreasonOfSaruman
legolasTheTreasonOfSaruman = ally LegolasTheTreasonOfSaruman Cards.legolasTheTreasonOfSaruman

-- | Response: After Legolas participates in an attack that destroys an enemy, draw 1 card.
instance HasAbilities LegolasTheTreasonOfSaruman where
  getAbilities (LegolasTheTreasonOfSaruman a) = [mkAbility a 1 $ response $ EnemyDestroyedByAttackWindow After (Just $ toTarget a)]

instance RunMessage LegolasTheTreasonOfSaruman where
  runMessage msg s@(LegolasTheTreasonOfSaruman a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> LegolasTheTreasonOfSaruman <$> runMessage msg a
