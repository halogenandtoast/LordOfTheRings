module LordOfTheRings.Location.Cards.TharbadHideout (tharbadHideout) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype TharbadHideout = TharbadHideout LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

tharbadHideout :: LocationCard TharbadHideout
tharbadHideout = location TharbadHideout Cards.tharbadHideout

-- | Surge. While Tharbad Hideout is the active location, time counters cannot be removed
-- from the current quest.
instance HasModifiersFor TharbadHideout where
  getModifiersFor (TharbadHideout a) =
    when (a.placement == ActiveLocation) do
      getCurrentQuest >>= traverse_ \q -> modified a q [CannotRemoveTimeCounters]

-- | Forced: At the beginning of the quest phase, place 1 progress on Tharbad Hideout.
instance HasAbilities TharbadHideout where
  getAbilities (TharbadHideout a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]

instance RunMessage TharbadHideout where
  runMessage msg l@(TharbadHideout attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeProgress attrs attrs 1
      pure l
    _ -> TharbadHideout <$> runMessage msg attrs
