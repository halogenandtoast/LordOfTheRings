module LordOfTheRings.Objective.Cards.GrimbeornTheOld (grimbeornTheOld) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Sphere (Sphere (Lore))

newtype GrimbeornTheOld = GrimbeornTheOld ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

grimbeornTheOld :: ObjectiveCard GrimbeornTheOld
grimbeornTheOld = objective GrimbeornTheOld Cards.grimbeornTheOld

-- | Action: Spend 1 Lore resource to place that resource on Grimbeorn the Old.
--
-- ponytail: the first player is the one offered the action, as with every encounter
-- card's ability.
instance HasAbilities GrimbeornTheOld where
  getAbilities (GrimbeornTheOld a) =
    [restricted a 1 (ResourcesOfSphere FirstPlayer Lore 1) $ ActionAbility Free | a.placement == StagingArea]

-- | If Grimbeorn the Old has 8 or more resource tokens on him, he joins the first player as
-- an ally.
instance RunMessage GrimbeornTheOld where
  runMessage msg o@(GrimbeornTheOld attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      spendResources pid Lore 1
      placeResources attrs attrs 1
      pure o
    PlaceTokens _ (isTarget attrs -> True) Resource n | attrs.resources + n >= 8 -> do
      lead <- getLead
      putObjectiveAllyIntoPlay attrs.id lead
      GrimbeornTheOld <$> runMessage msg attrs
    _ -> GrimbeornTheOld <$> runMessage msg attrs
