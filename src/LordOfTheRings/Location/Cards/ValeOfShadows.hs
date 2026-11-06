module LordOfTheRings.Location.Cards.ValeOfShadows (valeOfShadows) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation, StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight)

newtype ValeOfShadows = ValeOfShadows LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

valeOfShadows :: LocationCard ValeOfShadows
valeOfShadows = location ValeOfShadows Cards.valeOfShadows

instance HasModifiersFor ValeOfShadows where
  getModifiersFor (ValeOfShadows a) = do
    time <- currentNight
    let bonus = case time of Just Midnight -> 4; Just Dusk -> 2; _ -> 0
    modifySelf a [QuestPointModifier bonus]

instance RunMessage ValeOfShadows where
  runMessage msg l@(ValeOfShadows a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      active <- select IsActiveLocation
      for_ (filter (/= a.id) active) (\lid -> placeLocation lid StagingArea)
      placeLocation a.id ActiveLocation
      pure l
    _ -> ValeOfShadows <$> runMessage msg a
