module LordOfTheRings.Location.Cards.PatchOfMidnight (patchOfMidnight) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (poisonedCharacters)

newtype PatchOfMidnight = PatchOfMidnight LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

patchOfMidnight :: LocationCard PatchOfMidnight
patchOfMidnight = location PatchOfMidnight Cards.patchOfMidnight

instance HasModifiersFor PatchOfMidnight where
  getModifiersFor (PatchOfMidnight a) = when (a.placement == ActiveLocation) do
    poisoned <- poisonedCharacters
    for_ poisoned \c -> modified a c [Blank]

instance RunMessage PatchOfMidnight where
  runMessage msg l@(PatchOfMidnight a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      poisoned <-
        selectCharacters
          (PoisonedHero <> HeroControlledBy (PlayerWithId pid))
          (PoisonedAlly <> AllyControlledBy (PlayerWithId pid))
      raiseThreat pid (length poisoned)
      pure l
    _ -> PatchOfMidnight <$> runMessage msg a
