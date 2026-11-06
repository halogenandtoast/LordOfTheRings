module LordOfTheRings.Location.Cards.FishermansDock (fishermansDock) where

import LordOfTheRings.Helpers.Window (exhaustableHeroes)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype FishermansDock = FishermansDock LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

fishermansDock :: LocationCard FishermansDock
fishermansDock = location FishermansDock Cards.fishermansDock

instance HasModifiersFor FishermansDock where
  getModifiersFor (FishermansDock a) = do
    active <- select IsActiveLocation
    heroes <- exhaustableHeroes AnyHero
    modifySelf a $ [TravelableWithActiveLocation] <> [CannotBeTraveledTo | length active >= 2 || (length active == 1 && null heroes)]

instance RunMessage FishermansDock where
  runMessage msg l@(FishermansDock a) = case msg of
    TravelTo lid | lid == a.id -> do
      active <- select IsActiveLocation
      when (length active == 1) do
        heroes <- exhaustableHeroes AnyHero
        lead <- getLead
        chooseOneM lead do
          questionLabeled "lakeTown.dockHero"
          causedBy a
          targets heroes exhaust
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> l <$ putShadowLocationIntoPlay eid cid
    _ -> FishermansDock <$> runMessage msg a
