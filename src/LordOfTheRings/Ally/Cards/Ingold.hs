module LordOfTheRings.Ally.Cards.Ingold (ingold) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype Ingold = Ingold AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

ingold :: AllyCard Ingold
ingold = ally Ingold Cards.ingold

-- | Ingold gets +1 willpower for each hero you control with at least 1 resource in its
-- resource pool.
instance HasModifiersFor Ingold where
  getModifiersFor (Ingold a) = for_ (placementController a.placement) \pid -> do
    n <- selectCount (HeroControlledBy (PlayerWithId pid) <> HeroWithResources)
    when (n > 0) $ modifySelf a [WillpowerModifier n]
