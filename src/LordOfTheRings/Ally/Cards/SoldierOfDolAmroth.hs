module LordOfTheRings.Ally.Cards.SoldierOfDolAmroth (soldierOfDolAmroth) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype SoldierOfDolAmroth = SoldierOfDolAmroth AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

soldierOfDolAmroth :: AllyCard SoldierOfDolAmroth
soldierOfDolAmroth = ally SoldierOfDolAmroth Cards.soldierOfDolAmroth

-- | Response: After Soldier of Dol Amroth enters play, reduce the cost of the next tactics
-- card you play this phase by 1.
instance HasAbilities SoldierOfDolAmroth where
  getAbilities (SoldierOfDolAmroth a) = [mkAbility a 1 $ response $ entersPlay a]

instance RunMessage SoldierOfDolAmroth where
  runMessage msg s@(SoldierOfDolAmroth a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      phaseModifier a (PlayerTarget pid) (LowerNextCostOf (CardWithSphere Tactics) 1)
      pure s
    _ -> SoldierOfDolAmroth <$> runMessage msg a
