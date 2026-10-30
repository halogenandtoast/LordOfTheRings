module LordOfTheRings.Ally.Cards.RhovanionOutrider (rhovanionOutrider) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype RhovanionOutrider = RhovanionOutrider AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rhovanionOutrider :: AllyCard RhovanionOutrider
rhovanionOutrider = ally RhovanionOutrider Cards.rhovanionOutrider

staged :: LocationMatcher
staged = LocationInStagingArea <> LocationNotImmuneToPlayerCards

-- | Response: After Rhovanion Outrider commits to the quest, place 1 progress on a
-- location in the staging area. If that location is not explored by this effect, give
-- Rhovanion Outrider +1 willpower until the end of the phase.
instance HasAbilities RhovanionOutrider where
  getAbilities (RhovanionOutrider a) =
    [restricted a 1 (exists staged) $ response $ CommittedToQuestWindow After (toTarget a)]

instance RunMessage RhovanionOutrider where
  runMessage msg s@(RhovanionOutrider a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      locations <- select staged
      chooseOrRunOneM pid do
        questionLabeled "rhovanionOutrider.location"
        causedBy a
        targets locations \l -> do
          placeProgress a l 1
          -- explored, it has left play by now
          whileInPlay l $ phaseModifier a a (WillpowerModifier 1)
      pure s
    _ -> RhovanionOutrider <$> runMessage msg a
