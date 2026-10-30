module LordOfTheRings.Treachery.Cards.AimlessWandering (aimlessWandering) where

import LordOfTheRings.Helpers.Character (getCommittedCharacters, getWillpower)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AimlessWandering = AimlessWandering TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aimlessWandering :: TreacheryCard AimlessWandering
aimlessWandering = treachery AimlessWandering Cards.aimlessWandering

{- | When Revealed: The players, as a group, must remove characters with a total of at
least X willpower from the quest. X is the number of Uncharted locations in the staging
area. The first player may give Aimless Wandering doomed 2 to look at the facedown side of
an Uncharted location in the staging area.

The first player removes them one at a time, the willpower still to remove carried in the
step, until it is all removed or no character is left on the quest.
-}
instance RunMessage AimlessWandering where
  runMessage msg t@(AimlessWandering attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      islands <- select (LocationInStagingArea <> unchartedLocation)
      doStep (length islands) msg
      lead <- getLead
      unless (null islands) $ chooseOneM lead do
        questionLabeled "aimlessWandering.look"
        causedBy attrs
        labeled "aimlessWandering.doomed" do
          raiseThreatFromDoomed 2
          lookAtAnUnchartedLocation lead attrs
        labeled "aimlessWandering.decline" $ pure ()
      pure t
    DoStep x inner@(WhenRevealed (isTarget attrs -> True)) | x > 0 -> do
      lead <- getLead
      committed <- getCommittedCharacters
      unless (null committed) $ chooseOneM lead do
        questionLabeledWith "aimlessWandering.remove" [intVar "count" x]
        causedBy attrs
        targets committed \c -> do
          willpower <- getWillpower c
          removeFromQuest c
          doStep (x - willpower) inner
      pure t
    _ -> AimlessWandering <$> runMessage msg attrs
