module LordOfTheRings.Ally.Cards.Denethor (denethor) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype Denethor = Denethor AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

denethor :: AllyCard Denethor
denethor = ally Denethor Cards.denethor

-- | Denethor gets -1 willpower for each damaged hero you control. Discard Denethor if his
-- willpower is 0 or less.
instance HasModifiersFor Denethor where
  getModifiersFor (Denethor a) = for_ (placementController a.placement) \pid -> do
    heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
    damaged <- length <$> filterM (fmap (> 0) . getDamage) heroes
    modifySelf a $ DiscardedAtNoWillpower : [WillpowerModifier (negate damaged) | damaged > 0]
