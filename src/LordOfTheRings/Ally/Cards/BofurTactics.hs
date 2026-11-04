module LordOfTheRings.Ally.Cards.BofurTactics (bofurTactics) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (AttachmentType))
import LordOfTheRings.Trait (Trait (Weapon))

newtype BofurTactics = BofurTactics AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bofurTactics :: AllyCard BofurTactics
bofurTactics = ally BofurTactics Cards.bofurTactics

-- | Action: Exhaust Bofur to search the top 5 cards of your deck for 1 Weapon attachment.
-- Add that card to your hand and shuffle the other cards back into your deck.
instance HasAbilities BofurTactics where
  getAbilities (BofurTactics a) = [mkAbility a 1 $ ActionAbility (exhaustCost a)]

instance RunMessage BofurTactics where
  runMessage msg s@(BofurTactics a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchDeck pid a 5 (CardWithTrait Weapon <> CardWithType AttachmentType) False
      pure s
    _ -> BofurTactics <$> runMessage msg a
