module LordOfTheRings.Ally.Cards.MasterOfTheForge (masterOfTheForge) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (AttachmentType))

newtype MasterOfTheForge = MasterOfTheForge AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

masterOfTheForge :: AllyCard MasterOfTheForge
masterOfTheForge = ally MasterOfTheForge Cards.masterOfTheForge

-- | Action: Exhaust Master of the Forge to search the top 5 cards of your deck for any 1
-- attachment and add it to your hand. Shuffle the other cards back into your deck.
instance HasAbilities MasterOfTheForge where
  getAbilities (MasterOfTheForge a) = [mkAbility a 1 $ ActionAbility (exhaustCost a)]

instance RunMessage MasterOfTheForge where
  runMessage msg s@(MasterOfTheForge a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchDeck pid a 5 (CardWithType AttachmentType) False
      pure s
    _ -> MasterOfTheForge <$> runMessage msg a
