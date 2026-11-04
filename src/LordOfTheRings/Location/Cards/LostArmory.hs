module LordOfTheRings.Location.Cards.LostArmory (lostArmory) where

import LordOfTheRings.Card (CardType (AttachmentType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Armor, Weapon))

newtype LostArmory = LostArmory LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostArmory :: LocationCard LostArmory
lostArmory = location LostArmory Cards.lostArmory

-- | Response: After Lost Armory becomes the active location, each player may search his deck
-- for a Weapon or Armor attachment, add it to his hand, and shuffle his deck. Limit once per
-- game for the group.
instance HasAbilities LostArmory where
  getAbilities (LostArmory a) =
    [limited (GroupLimit PerGame 1) $ mkAbility a 1 $ response $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage LostArmory where
  runMessage msg l@(LostArmory a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> do
        deck <- (.deck) <$> getPlayer p
        searchDeck p a (length deck) gear False
      pure l
    _ -> LostArmory <$> runMessage msg a
   where
    gear = CardWithType AttachmentType <> CardMatchAny [CardWithTrait Weapon, CardWithTrait Armor]
