module LordOfTheRings.Objective.Cards.Wormsbane (wormsbane, wormsbaneReforged) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers (dagnirX)

-- | Mount Gundabad's Wormsbane: one card, Broken on one side and Reforged on the other, the
-- side faceup kept as its card code.
newtype Wormsbane = Wormsbane ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

wormsbane :: ObjectiveCard Wormsbane
wormsbane = objective Wormsbane Cards.wormsbane

wormsbaneReforged :: ObjectiveCard Wormsbane
wormsbaneReforged = objective Wormsbane Cards.wormsbaneReforged

isReforged :: ObjectiveAttrs -> Bool
isReforged a = toCardCode a == toCardCode Cards.wormsbaneReforged

{- | Broken: Attached hero gets +1 attack. Reforged: Attached hero gets +X attack, where X is
1 more than the number of quest cards in the victory display.
Discarded, as the restricted limit may discard it from a hero still in play, it goes to the
encounter discard pile: the rule sheet and FAQ 1.9 say nothing of it, and neither side of the
card says to return it to the staging area, so the core rules apply: a discarded card goes
to the discard pile of the deck it belongs to, and Wormsbane is an encounter card, an
objective of the Mount Gundabad encounter set (as Hall of Beorn prints it).
-}
instance HasModifiersFor Wormsbane where
  getModifiersFor (Wormsbane a) = do
    modifySelf a [LeavesPlayToEncounterDiscard]
    case a.placement of
      AttachedTo host -> do
        x <- if isReforged a then dagnirX else pure 1
        modified a host [AttackModifier x]
      _ -> pure ()

-- | If attached hero leaves play, the players lose the game: the hero gone, what was
-- attached to it is discarded. Wormsbane discarded from a hero still in play, as The Great
-- Worm or the restricted limit discards it, loses nothing.
instance RunMessage Wormsbane where
  runMessage msg (Wormsbane attrs) = case msg of
    Discard _ (isTarget attrs -> True) | AttachedTo (HeroTarget hid) <- attrs.placement -> do
      inPlay <- getsGame (Map.member hid . (.heroes) . gameEntities)
      unless inPlay $ gameOver Lost
      Wormsbane <$> runMessage msg attrs
    Flip (isTarget attrs -> True) -> do
      let side = if isReforged attrs then Cards.wormsbane else Cards.wormsbaneReforged
      pure $ Wormsbane $ attrs {objectiveCardCode = toCardCode side}
    _ -> Wormsbane <$> runMessage msg attrs
