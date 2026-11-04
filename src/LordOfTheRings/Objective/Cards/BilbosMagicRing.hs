module LordOfTheRings.Objective.Cards.BilbosMagicRing (bilbosMagicRing) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim.Helpers (riddleDiscard)
import LordOfTheRings.Sphere (Sphere (Baggins))

newtype BilbosMagicRing = BilbosMagicRing ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bilbosMagicRing :: ObjectiveCard BilbosMagicRing
bilbosMagicRing = objective BilbosMagicRing Cards.bilbosMagicRing

{- | Attach to Bilbo Baggins.
Action: When answering a riddle, spend 1 Baggins resource to discard an additional player
card from the top of your deck.
Action: Exhaust Bilbo's Magic Ring and raise each player's threat by 2 to add 1 Baggins
resource to Bilbo Baggins' resource pool.

A riddle is being answered once its cards are discarded and before its matches are counted
(FAQ: "Players can take actions after step 2, before step 3").
-}
instance HasAbilities BilbosMagicRing where
  getAbilities (BilbosMagicRing a) = case a.placement of
    AttachedTo _ ->
      [ restricted a 1 (TargetHasModifier GameTarget AnsweringRiddle) $ ActionAbility (SpendResourcesCost Baggins 1)
      , mkAbility a 2 $ ActionAbility (ExhaustCost $ toTarget a)
      ]
    _ -> []

instance RunMessage BilbosMagicRing where
  runMessage msg o@(BilbosMagicRing attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      riddleDiscard 1
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      eachPlayer \p -> raiseThreat p 2
      getBilbo >>= traverse_ \b -> placeResources attrs b 1
      pure o
    _ -> BilbosMagicRing <$> runMessage msg attrs
