module LordOfTheRings.Location.Cards.SharpJetties (sharpJetties) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype SharpJetties = SharpJetties LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sharpJetties :: LocationCard SharpJetties
sharpJetties = location SharpJetties Cards.sharpJetties

-- | Forced: When any amount of progress is placed on Sharp Jetties, either remove an equal
-- amount of progress from the current quest, or deal an equal amount of damage to each
-- Ship objective.
instance HasAbilities SharpJetties where
  getAbilities (SharpJetties a) = [mkAbility a 1 $ forced $ ProgressPlacedWindow After (toTarget a)]

-- | When Revealed: If your heading is off-course, make Sharp Jetties the active location,
-- returning any currently active location to the staging area.
instance RunMessage SharpJetties where
  runMessage msg l@(SharpJetties a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      whenM isOffCourse do
        active <- select IsActiveLocation
        for_ active \x -> placeLocation x StagingArea
        makeActive a.id
      pure l
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      let n = sum [k | Window _ (ProgressPlaced _ k) <- ws]
      lead <- getLead
      ships <- select shipObjective
      chooseOneM lead do
        questionLabeled "sharpJetties.choose"
        causedBy a
        labeled "sharpJetties.removeProgress" $ removeProgressFromQuest a n
        labeled "sharpJetties.damageShips" $ for_ ships \s -> dealDamage a s n
      pure l
    _ -> SharpJetties <$> runMessage msg a
