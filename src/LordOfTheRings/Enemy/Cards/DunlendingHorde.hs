module LordOfTheRings.Enemy.Cards.DunlendingHorde (dunlendingHorde) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype DunlendingHorde = DunlendingHorde EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunlendingHorde :: EnemyCard DunlendingHorde
dunlendingHorde = enemy DunlendingHorde Cards.dunlendingHorde

-- | Cannot have attachments or take non-combat damage.
instance HasModifiersFor DunlendingHorde where
  getModifiersFor (DunlendingHorde a) = modifySelf a [CannotHaveAttachments, CannotTakeNonCombatDamage]

-- | Forced: After the engaged player draws any number of cards, heal all damage from
-- Dunlending Horde.
instance HasAbilities DunlendingHorde where
  getAbilities (DunlendingHorde a) =
    [mkAbility a 1 $ forced $ DrewCardsWindow After (Just pid) | EngagedWith pid <- [a.placement]]

instance RunMessage DunlendingHorde where
  runMessage msg e@(DunlendingHorde attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      healAllDamage attrs attrs.id
      pure e
    _ -> DunlendingHorde <$> runMessage msg attrs
