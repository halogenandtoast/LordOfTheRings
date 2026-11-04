module LordOfTheRings.Location.Cards.Chetwood (chetwood) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Spy))

newtype Chetwood = Chetwood LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

chetwood :: LocationCard Chetwood
chetwood = location Chetwood Cards.chetwood

-- | While Chetwood is in the staging area, Spy enemies get +1 threat and +1 attack.
instance HasModifiersFor Chetwood where
  getModifiersFor (Chetwood a) = when (a.placement == StagingArea) do
    spies <- select (EnemyWithTrait Spy)
    for_ spies \e -> modified a e [ThreatModifier 1, AttackModifier 1]

-- | Shadow: Defending player discards 1 random card from his hand.
instance RunMessage Chetwood where
  runMessage msg l@(Chetwood a) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      discardRandomFromHand pid 1
      pure l
    _ -> Chetwood <$> runMessage msg a
