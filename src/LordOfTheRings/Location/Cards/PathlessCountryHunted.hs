module LordOfTheRings.Location.Cards.PathlessCountryHunted (pathlessCountryHunted) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype PathlessCountryHunted = PathlessCountryHunted LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

pathlessCountryHunted :: LocationCard PathlessCountryHunted
pathlessCountryHunted = location PathlessCountryHunted Cards.pathlessCountryHunted

-- | Pathless Country gets +4 quest points while in the staging area.
instance HasModifiersFor PathlessCountryHunted where
  getModifiersFor (PathlessCountryHunted a) = when (a.placement == StagingArea) $ modifySelf a [QuestPointModifier 4]

-- | Shadow: After this attack, the attacking enemy engages the next player then makes an
-- immediate attack. With no next player, nothing.
instance RunMessage PathlessCountryHunted where
  runMessage msg l@(PathlessCountryHunted a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      getNextPlayer pid >>= traverse_ \next -> do
        again <- capture do
          engageEnemy next eid
          immediateAttack next eid
        afterAttack eid again
      pure l
    _ -> PathlessCountryHunted <$> runMessage msg a
