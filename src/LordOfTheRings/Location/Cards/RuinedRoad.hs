module LordOfTheRings.Location.Cards.RuinedRoad (ruinedRoad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype RuinedRoad = RuinedRoad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ruinedRoad :: LocationCard RuinedRoad
ruinedRoad = location RuinedRoad Cards.ruinedRoad

-- | Response: After you travel to Ruined Road, the first player places 2 progress tokens
-- on it or readies 1 hero he controls.
instance HasAbilities RuinedRoad where
  getAbilities (RuinedRoad a) = [mkAbility a 1 $ response $ TraveledWindow After a.id]

-- | Shadow: Return attacking enemy to the staging area after it attacks.
instance RunMessage RuinedRoad where
  runMessage msg l@(RuinedRoad a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      exhausted <- select (HeroControlledBy (PlayerWithId lead) <> ExhaustedHero)
      chooseOneM lead do
        questionLabeled "ruinedRoad.choose"
        causedBy a
        labeled "ruinedRoad.progress" $ placeProgress a a 2
        targets exhausted ready
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> RuinedRoad <$> runMessage msg a
