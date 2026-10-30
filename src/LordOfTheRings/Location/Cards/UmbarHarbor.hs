module LordOfTheRings.Location.Cards.UmbarHarbor (umbarHarbor) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Trait (Trait (Corsair, Raider))

newtype UmbarHarbor = UmbarHarbor LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

umbarHarbor :: LocationCard UmbarHarbor
umbarHarbor = location UmbarHarbor Cards.umbarHarbor

-- | While Umbar Harbor is the active location, Corsair enemies get +2 defense.
instance HasModifiersFor UmbarHarbor where
  getModifiersFor (UmbarHarbor a) = when (a.placement == ActiveLocation) do
    corsairs <- select (EnemyWithTrait Corsair)
    for_ corsairs \c -> modified a c [DefenseModifier 2]

-- | Shadow: If attacking enemy is a Raider, move 1 resource from a hero you control to
-- attacking enemy.
instance RunMessage UmbarHarbor where
  runMessage msg l@(UmbarHarbor a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      whenM (selectAny $ EnemyWithId eid <> EnemyWithTrait Raider) do
        heroes <- select (HeroControlledBy (PlayerWithId pid) <> HeroWithResources)
        chooseOrRunOneM pid do
          questionLabeled "takeResourceFromAHero"
          causedBy a
          targets heroes \h -> do
            removeTokens a h Resource 1
            placeResources a eid 1
      pure l
    _ -> UmbarHarbor <$> runMessage msg a
