module LordOfTheRings.Location.Cards.FrozenSilverlode (frozenSilverlode) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype FrozenSilverlode = FrozenSilverlode LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

frozenSilverlode :: LocationCard FrozenSilverlode
frozenSilverlode = location FrozenSilverlode Cards.frozenSilverlode

-- | While Frozen Silverlode is in the staging area, characters cannot gain [Willpower]
-- from player card effects.
instance HasModifiersFor FrozenSilverlode where
  getModifiersFor (FrozenSilverlode a) = when (a.placement == StagingArea) $ modified a GameTarget [NoWillpowerFromPlayerCards]

-- | Shadow: Defending character gets -1 [Willpower]. Deal the attacking enemy 1 additional
-- shadow card.
instance RunMessage FrozenSilverlode where
  runMessage msg l@(FrozenSilverlode a) = case msg of
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      for_ defenders \d -> attackModifier a d (WillpowerModifier (-1))
      dealShadowCardsDuringAttack eid pid defenders 1
      pure l
    _ -> FrozenSilverlode <$> runMessage msg a
