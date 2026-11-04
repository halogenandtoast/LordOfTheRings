module LordOfTheRings.Ally.Cards.SouthronRefugee (southronRefugee) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Trait (Trait (Harad))

newtype SouthronRefugee = SouthronRefugee AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronRefugee :: AllyCard SouthronRefugee
southronRefugee = ally SouthronRefugee Cards.southronRefugee

-- | Action: Exhaust Southron Refugee to reduce the cost of the next Harad ally you play
-- this phase by 1.
instance HasAbilities SouthronRefugee where
  getAbilities (SouthronRefugee a) = [mkAbility a 1 $ ActionAbility (exhaustCost a)]

instance RunMessage SouthronRefugee where
  runMessage msg s@(SouthronRefugee a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      phaseModifier a (PlayerTarget pid) (LowerNextCostOf (CardWithType AllyType <> CardWithTrait Harad) 1)
      pure s
    _ -> SouthronRefugee <$> runMessage msg a
