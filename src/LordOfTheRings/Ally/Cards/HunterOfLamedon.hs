module LordOfTheRings.Ally.Cards.HunterOfLamedon (hunterOfLamedon) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Outlands))

newtype HunterOfLamedon = HunterOfLamedon AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hunterOfLamedon :: AllyCard HunterOfLamedon
hunterOfLamedon = ally HunterOfLamedon Cards.hunterOfLamedon

-- | Response: After you play Hunter of Lamedon from your hand, reveal the top card of
-- your deck. If it is an Outlands card, add it to your hand. Otherwise, discard it.
instance HasAbilities HunterOfLamedon where
  getAbilities (HunterOfLamedon a) = [mkAbility a 1 $ response $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)]

instance RunMessage HunterOfLamedon where
  runMessage msg s@(HunterOfLamedon a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      top <- headMay . (.deck) <$> getPlayer pid
      -- added to hand, not drawn, so what stops drawing does not stop it
      for_ top \c -> if cardMatches (CardWithTrait Outlands) c then takeFromDeck pid c else discardTopOfDeck pid 1
      pure s
    _ -> HunterOfLamedon <$> runMessage msg a
