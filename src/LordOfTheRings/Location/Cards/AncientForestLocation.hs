module LordOfTheRings.Location.Cards.AncientForestLocation (ancientForestLocation) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Forest))

newtype AncientForestLocation = AncientForestLocation LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ancientForestLocation :: LocationCard AncientForestLocation
ancientForestLocation = location AncientForestLocation Cards.ancientForestLocation

-- | While Ancient Forest is in the staging area, each Forest location in the staging area
-- gets +1 threat and +3 quest points. This ability does not stack with other copies of
-- Ancient Forest: only the first copy in the staging area gives it.
instance HasModifiersFor AncientForestLocation where
  getModifiersFor (AncientForestLocation a) = when (a.placement == StagingArea) do
    copies <- select (LocationWithCardCode (toCardCode Cards.ancientForestLocation) <> LocationInStagingArea)
    when (minimumMay copies == Just a.id) do
      forests <- select (LocationWithTrait Forest <> LocationInStagingArea)
      for_ forests \l -> modified a l [ThreatModifier 1, QuestPointModifier 3]

-- | Shadow: Defending player exhausts a character he controls.
instance RunMessage AncientForestLocation where
  runMessage msg l@(AncientForestLocation a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      chooseAndExhaust a pid 1
      pure l
    _ -> AncientForestLocation <$> runMessage msg a
