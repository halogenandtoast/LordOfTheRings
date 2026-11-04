module LordOfTheRings.Location.Cards.AncientTreasury (ancientTreasury) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype AncientTreasury = AncientTreasury LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ancientTreasury :: LocationCard AncientTreasury
ancientTreasury = location AncientTreasury Cards.ancientTreasury

-- | Response: After Ancient Treasury becomes the active location, each player may (choose
-- one): reduce his threat by 3, draw 2 cards, or add 1 resource to a hero's resource pool.
-- Limit once per game for the group.
instance HasAbilities AncientTreasury where
  getAbilities (AncientTreasury a) =
    [limited (GroupLimit PerGame 1) $ mkAbility a 1 $ response $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage AncientTreasury where
  runMessage msg l@(AncientTreasury a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> do
        heroes <- select (HeroControlledBy $ PlayerWithId p)
        chooseOneM p do
          questionLabeled "ancientTreasury.choose"
          causedBy a
          labeled "ancientTreasury.threat" $ lowerThreatBy a p 3
          labeled "ancientTreasury.draw" $ drawCards a p 2
          unless (null heroes) $ labeled "ancientTreasury.resource" do
            chooseOrRunOneM p do
              questionLabeled "ancientTreasury.hero"
              causedBy a
              targets heroes \h -> placeResources a h 1
          labeled "pass" (pure ())
      pure l
    _ -> AncientTreasury <$> runMessage msg a
