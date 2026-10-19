module LordOfTheRings.Ally.Cards.Gleowine (gleowine) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Gleowine = Gleowine AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gleowine :: AllyCard Gleowine
gleowine = ally Gleowine Cards.gleowine

-- | Action: Exhaust Gléowine to choose a player. That player draws 1 card.
instance HasAbilities Gleowine where
  getAbilities (Gleowine a) = [mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a)]

instance RunMessage Gleowine where
  runMessage msg s@(Gleowine a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      choosePlayerM pid a \p -> drawCards p 1
      pure s
    _ -> Gleowine <$> runMessage msg a
