module LordOfTheRings.Ally.Cards.Faramir (faramir) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Faramir = Faramir AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

faramir :: AllyCard Faramir
faramir = ally Faramir Cards.faramir

-- | Action: Exhaust Faramir to choose a player. Each character controlled by that player
-- gets +1 willpower until the end of the phase.
instance HasAbilities Faramir where
  getAbilities (Faramir a) = [mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a)]

instance RunMessage Faramir where
  runMessage msg f@(Faramir attrs) = case msg of
    UseCardAbility p (isSource attrs -> True) 1 _ -> do
      choosePlayerM p attrs \pid -> do
        characters <- charactersControlledBy pid
        for_ characters \c -> phaseModifier attrs c (WillpowerModifier 1)
      pure f
    _ -> Faramir <$> runMessage msg attrs
