module LordOfTheRings.Location.Cards.StreetsOfTharbad (streetsOfTharbad) where

import LordOfTheRings.Classes.HasQueue (fromQueue)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation, StagingArea))

newtype StreetsOfTharbad = StreetsOfTharbad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

streetsOfTharbad :: LocationCard StreetsOfTharbad
streetsOfTharbad = location StreetsOfTharbad Cards.streetsOfTharbad

-- | While Streets of Tharbad is the active location, enemies get -20 engagement cost.
instance HasModifiersFor StreetsOfTharbad where
  getModifiersFor (StreetsOfTharbad a) =
    when (a.placement == ActiveLocation) do
      enemies <- select AnyEnemy
      for_ enemies \e -> modified a e [EngagementCostModifier (-20)]

-- | Progress cannot be placed on Streets of Tharbad while it is in the staging area. While
-- it is still being revealed it is not there yet, so Warden of Arnor's progress lands (FAQ
-- 3.188): its revealing ends with the window after it is revealed.
instance RunMessage StreetsOfTharbad where
  runMessage msg l@(StreetsOfTharbad attrs) = case msg of
    PlaceTokens _ (isTarget attrs -> True) Progress _ | attrs.placement == StagingArea -> do
      let revealing = \case
            CheckWindows ws -> Window After (Revealed $ toTarget attrs) `elem` ws
            _ -> False
      beingRevealed <- lift $ fromQueue (any revealing)
      if beingRevealed then StreetsOfTharbad <$> runMessage msg attrs else pure l
    _ -> StreetsOfTharbad <$> runMessage msg attrs
