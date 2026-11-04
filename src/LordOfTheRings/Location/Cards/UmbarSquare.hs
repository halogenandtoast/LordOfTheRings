module LordOfTheRings.Location.Cards.UmbarSquare (umbarSquare) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype UmbarSquare = UmbarSquare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

umbarSquare :: LocationCard UmbarSquare
umbarSquare = location UmbarSquare Cards.umbarSquare

-- | While Umbar Square is the active location, it gains: "Response: After an enemy is
-- defeated, place 1 progress on the main quest, bypassing the active location."
instance HasAbilities UmbarSquare where
  getAbilities (UmbarSquare a) = [mkAbility a 1 $ response $ EnemyDestroyedWindow After AnyCard | a.placement == ActiveLocation]

-- | Travel: Reveal the top card of the encounter deck to travel here.
instance RunMessage UmbarSquare where
  runMessage msg l@(UmbarSquare a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealEncounterCard
      UmbarSquare <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeProgressOnMainQuest a 1
      pure l
    _ -> UmbarSquare <$> runMessage msg a
