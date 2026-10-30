module LordOfTheRings.Ally.Cards.GaldorOfTheHavens (galdorOfTheHavens) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype GaldorOfTheHavens = GaldorOfTheHavens AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

galdorOfTheHavens :: AllyCard GaldorOfTheHavens
galdorOfTheHavens = ally GaldorOfTheHavens Cards.galdorOfTheHavens

-- | Response: After 1 or more cards are discarded from your hand, draw 1 card. Limit once
-- per round.
instance HasAbilities GaldorOfTheHavens where
  getAbilities (GaldorOfTheHavens a) =
    [ limited (PlayerLimit PerRound 1) $ mkAbility a 1 $ response $ DiscardedFromHandWindow After pid
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage GaldorOfTheHavens where
  runMessage msg s@(GaldorOfTheHavens a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> GaldorOfTheHavens <$> runMessage msg a
