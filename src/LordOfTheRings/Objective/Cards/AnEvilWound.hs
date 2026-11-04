module LordOfTheRings.Objective.Cards.AnEvilWound (anEvilWound) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (reduceRingBearersLife)

newtype AnEvilWound = AnEvilWound ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to the Ring-bearer: stage 1A's setup attaches it.
anEvilWound :: ObjectiveCard AnEvilWound
anEvilWound = objective AnEvilWound Cards.anEvilWound

-- | Attached hero cannot be healed.
instance HasModifiersFor AnEvilWound where
  getModifiersFor (AnEvilWound a) = case a.placement of
    AttachedTo host -> modified a host [CannotBeHealed]
    _ -> pure ()

-- | When attached hero would take any amount of damage, reduce the Ring-bearer's life an
-- equal amount instead: answered as the damage is about to be dealt, in the window before
-- it, where it is canceled.
instance HasAbilities AnEvilWound where
  getAbilities (AnEvilWound a) = case a.placement of
    AttachedTo host -> [mkAbility a 1 $ forced $ DealtDamageWindow When host]
    _ -> []

-- | If the Ring-bearer's life reaches zero, the players lose the game: the scenario checks
-- it as the life falls.
instance RunMessage AnEvilWound where
  runMessage msg o@(AnEvilWound attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> case attrs.placement of
      AttachedTo host -> do
        for_ [n | Window _ (DealtDamage t n) <- ws, t == host] \n -> do
          cancelDamage host
          reduceRingBearersLife n
        pure o
      _ -> pure o
    _ -> AnEvilWound <$> runMessage msg attrs
