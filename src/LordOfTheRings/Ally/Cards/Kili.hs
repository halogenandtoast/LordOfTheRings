module LordOfTheRings.Ally.Cards.Kili (kili) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype Kili = Kili AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

kili :: AllyCard Kili
kili = ally Kili Cards.kili

-- | Response: After you play Kili from your hand during the planning phase, search your
-- deck for Fili and put him into play under your control. Then, shuffle your deck.
instance HasAbilities Kili where
  getAbilities (Kili a) =
    [restricted a 1 (DuringPhase PlanningPhase) $ response $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)]

instance RunMessage Kili where
  runMessage msg s@(Kili a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchDeckIntoPlay pid a maxBound (cardIs Cards.fili) 1 []
      pure s
    _ -> Kili <$> runMessage msg a
