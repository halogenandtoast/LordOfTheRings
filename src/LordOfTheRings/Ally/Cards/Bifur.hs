module LordOfTheRings.Ally.Cards.Bifur (bifur) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Bifur = Bifur AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bifur :: AllyCard Bifur
bifur = ally Bifur Cards.bifur

-- | While you control at least 5 Dwarf characters, Bifur gains: "Response: After you play
-- Bifur from your hand, draw 2 cards."
instance HasAbilities Bifur where
  getAbilities (Bifur a) =
    [ restricted a 1 (DwarvesAtLeast a.controller 5)
        $ response
        $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)
    ]

instance RunMessage Bifur where
  runMessage msg s@(Bifur a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 2
      pure s
    _ -> Bifur <$> runMessage msg a
