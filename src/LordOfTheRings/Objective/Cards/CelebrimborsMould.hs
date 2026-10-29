module LordOfTheRings.Objective.Cards.CelebrimborsMould (celebrimborsMould) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))

newtype CelebrimborsMould = CelebrimborsMould ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

celebrimborsMould :: ObjectiveCard CelebrimborsMould
celebrimborsMould = objective CelebrimborsMould Cards.celebrimborsMould

-- | If Celebrimbor's Mould is free of encounters, the first player claims it and attaches it
-- to a hero he controls. If the attached hero leaves play, the players lose the game.
-- Attached to The Secret Chamber or to Bellach it is not free; it is once that card leaves
-- play, which returns it to the staging area.
instance RunMessage CelebrimborsMould where
  runMessage msg (CelebrimborsMould attrs) = case msg of
    -- leaving its hero, the game is lost: no one claims it
    PlaceObjective oid StagingArea | oid == attrs.id, not onHero -> do
      lead <- getLead
      heroes <- select (HeroControlledBy $ PlayerWithId lead)
      chooseOrRunOneM lead do
        questionLabeled "celebrimborsMould.attach"
        causedBy attrs
        targets heroes $ attachObjective attrs.id
      CelebrimborsMould <$> runMessage msg attrs
    Discard (isTarget attrs -> True) | onHero -> do
      gameOver Lost
      CelebrimborsMould <$> runMessage msg attrs
    _ -> CelebrimborsMould <$> runMessage msg attrs
   where
    onHero = case attrs.placement of
      AttachedTo (HeroTarget _) -> True
      _ -> False
