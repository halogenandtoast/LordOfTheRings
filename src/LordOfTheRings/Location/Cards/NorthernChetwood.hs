module LordOfTheRings.Location.Cards.NorthernChetwood (northernChetwood) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Quest.CardDefs qualified as Quests

newtype NorthernChetwood = NorthernChetwood LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

northernChetwood :: LocationCard NorthernChetwood
northernChetwood = location NorthernChetwood Cards.northernChetwood

-- | Forced: After progress is placed on Northern Chetwood (and it is still in play),
-- search the encounter deck, discard pile and victory display for Lost in the Wilderness,
-- and reveal it.
instance HasAbilities NorthernChetwood where
  getAbilities (NorthernChetwood a) = [mkAbility a 1 $ forced $ ProgressPlacedWindow After (toTarget a)]

-- | Shadow: Discard 1 card at random from your hand for each point of damage dealt by this
-- attack.
instance RunMessage NorthernChetwood where
  runMessage msg l@(NorthernChetwood a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      searchAndRevealEncounterCard (cardIs Quests.lostInTheWilderness)
      pure l
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      attackModifier a eid (AttackDamageDiscardsRandomFromHandOf pid)
      pure l
    _ -> NorthernChetwood <$> runMessage msg a
