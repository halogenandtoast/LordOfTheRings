module LordOfTheRings.Ally.Cards.FornostBowman (fornostBowman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype FornostBowman = FornostBowman AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged, on its card def.
fornostBowman :: AllyCard FornostBowman
fornostBowman = ally FornostBowman Cards.fornostBowman

-- | Fornost Bowman gets +1 attack for each enemy engaged with you.
instance HasModifiersFor FornostBowman where
  getModifiersFor (FornostBowman a) = for_ (placementController a.placement) \pid -> do
    engaged <- length <$> select (EnemyEngagedWith $ PlayerWithId pid)
    modifySelf a [AttackModifier engaged | engaged > 0]
