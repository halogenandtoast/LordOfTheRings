module LordOfTheRings.Location.Cards.WoodedRiverbank (woodedRiverbank) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype WoodedRiverbank = WoodedRiverbank LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

woodedRiverbank :: LocationCard WoodedRiverbank
woodedRiverbank = location WoodedRiverbank Cards.woodedRiverbank

-- | Forced: When Wooded Riverbank is explored, put the top card of the Evil Creatures deck
-- into play engaged with the first player.
instance HasAbilities WoodedRiverbank where
  getAbilities (WoodedRiverbank a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

-- | Shadow: Deal 1 damage to the defending character.
instance RunMessage WoodedRiverbank where
  runMessage msg l@(WoodedRiverbank a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      putFromEvilCreaturesDeckEngaged a lead
      pure l
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders \d -> dealDamage a d 1
      pure l
    _ -> WoodedRiverbank <$> runMessage msg a
