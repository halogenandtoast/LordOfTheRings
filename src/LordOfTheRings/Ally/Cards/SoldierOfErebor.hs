module LordOfTheRings.Ally.Cards.SoldierOfErebor (soldierOfErebor) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype SoldierOfErebor = SoldierOfErebor AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

soldierOfErebor :: AllyCard SoldierOfErebor
soldierOfErebor = ally SoldierOfErebor Cards.soldierOfErebor

-- | Action: Discard the top 3 cards of your deck to ready Soldier of Erebor. (Limit once
-- per round.) The 3 cards are its cost, paid only with that many in the deck.
instance HasAbilities SoldierOfErebor where
  getAbilities (SoldierOfErebor a) =
    [ limited (PlayerLimit PerRound 1)
        $ restricted a 1 (exists (AllyWithId a.id <> ExhaustedAlly) <> PlayerExists (PlayerWithId a.controller <> PlayerWithCardsInDeck 3))
        $ ActionAbility Free
    ]

instance RunMessage SoldierOfErebor where
  runMessage msg s@(SoldierOfErebor a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      discardTopOfDeck pid 3
      ready a
      pure s
    _ -> SoldierOfErebor <$> runMessage msg a
