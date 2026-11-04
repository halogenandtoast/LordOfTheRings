module LordOfTheRings.Location.Cards.NarrowStair (narrowStair) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype NarrowStair = NarrowStair LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

narrowStair :: LocationCard NarrowStair
narrowStair = location NarrowStair Cards.narrowStair

-- | Deep. When faced with the option to travel, the players must travel to Narrow Stair.
instance HasModifiersFor NarrowStair where
  getModifiersFor (NarrowStair a) = when (a.placement == StagingArea) $ modifySelf a [MustBeTraveledTo]

-- | Shadow: Until the end of the round, defending character cannot be readied by player
-- card effects.
instance RunMessage NarrowStair where
  runMessage msg l@(NarrowStair a) = case msg of
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders \d -> roundModifier a d CannotBeReadiedByPlayerCards
      pure l
    _ -> NarrowStair <$> runMessage msg a
