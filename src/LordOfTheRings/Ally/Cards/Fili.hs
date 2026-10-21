module LordOfTheRings.Ally.Cards.Fili (fili) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype Fili = Fili AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fili :: AllyCard Fili
fili = ally Fili Cards.fili

-- | Response: After you play Fili from your hand during the planning phase, search your
-- deck for Kili and put him into play under your control. Then, shuffle your deck.
instance HasAbilities Fili where
  getAbilities (Fili a) =
    [restricted a 1 (DuringPhase PlanningPhase) $ response $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)]

instance RunMessage Fili where
  runMessage msg s@(Fili a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchDeckIntoPlay pid a maxBound (cardIs Cards.kili) 1 []
      pure s
    _ -> Fili <$> runMessage msg a
