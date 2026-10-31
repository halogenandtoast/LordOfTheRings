module LordOfTheRings.Location.Cards.CrumblingStairs (crumblingStairs) where

import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype CrumblingStairs = CrumblingStairs LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

crumblingStairs :: LocationCard CrumblingStairs
crumblingStairs = location CrumblingStairs Cards.crumblingStairs

-- | While Crumbling Stairs is in the staging area, event cards cost 1 additional matching
-- resource to play from hand.
instance HasModifiersFor CrumblingStairs where
  getModifiersFor (CrumblingStairs a) = when (a.placement == StagingArea) $ modified a GameTarget [IncreaseCost (CardWithType EventType) 1]

-- | Shadow: If this attack destroys a character, the defending player must discard all
-- event cards in his hand.
instance RunMessage CrumblingStairs where
  runMessage msg l@(CrumblingStairs a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == a.cardId -> do
      discardEventsFromHand pid
      pure l
    _ -> CrumblingStairs <$> runMessage msg a
