module LordOfTheRings.Ally.Cards.Beorn (beorn) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Beorn = Beorn AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

beorn :: AllyCard Beorn
beorn = ally Beorn Cards.beorn

-- | Action: Beorn gains +5 attack until the end of the phase. At the end of the phase in
-- which you trigger this effect, shuffle Beorn back into your deck. (Limit once per round.)
instance HasAbilities Beorn where
  getAbilities (Beorn a) = [limited (GroupLimit PerRound 1) $ mkAbility a 1 $ ActionAbility Free]

instance RunMessage Beorn where
  runMessage msg s@(Beorn a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      push $ CreateEffect $ Effect (toSource a) (toTarget a) [AttackModifier 5, ShuffleIntoDeckAtEndOfPhase] UntilEndOfPhase
      pure s
    _ -> Beorn <$> runMessage msg a
