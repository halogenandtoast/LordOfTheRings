module LordOfTheRings.Ally.Cards.Erestor (erestor) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype Erestor = Erestor AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

erestor :: AllyCard Erestor
erestor = ally Erestor Cards.erestor

-- | Action: Choose and discard 1 card from your hand to draw 1 card. (Limit once per
-- round.)
instance HasAbilities Erestor where
  getAbilities (Erestor a) =
    [ limited (GroupLimit PerRound 1) $ mkAbility a 1 $ ActionAbility (DiscardFromHandCost pid 1)
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage Erestor where
  runMessage msg s@(Erestor a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> Erestor <$> runMessage msg a
