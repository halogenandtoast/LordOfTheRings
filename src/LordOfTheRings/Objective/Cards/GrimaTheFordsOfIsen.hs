module LordOfTheRings.Objective.Cards.GrimaTheFordsOfIsen (grimaTheFordsOfIsen) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))

newtype GrimaTheFordsOfIsen = GrimaTheFordsOfIsen ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally attached to The Islet, until he is free of encounters.
grimaTheFordsOfIsen :: ObjectiveCard GrimaTheFordsOfIsen
grimaTheFordsOfIsen = objective GrimaTheFordsOfIsen Cards.grimaTheFordsOfIsen

-- | If free of encounters, the first player gains control of Gríma: as The Islet leaves
-- play he goes to the staging area, and from there to the first player.
-- If Gríma leaves play, the players lose the game: discarded while The Islet is still in
-- play, as Fight at the Fords discards him.
instance RunMessage GrimaTheFordsOfIsen where
  runMessage msg o@(GrimaTheFordsOfIsen attrs) = case msg of
    PlaceObjective oid StagingArea | oid == attrs.id -> do
      lead <- getLead
      putObjectiveAllyIntoPlay attrs.id lead
      GrimaTheFordsOfIsen <$> runMessage msg attrs
    Discard (isTarget attrs -> True) -> do
      hostInPlay <- case attrs.placement of
        AttachedTo (LocationTarget lid) -> selectAny (LocationWithId lid)
        _ -> pure True
      when hostInPlay $ gameOver Lost
      pure o
    _ -> GrimaTheFordsOfIsen <$> runMessage msg attrs
