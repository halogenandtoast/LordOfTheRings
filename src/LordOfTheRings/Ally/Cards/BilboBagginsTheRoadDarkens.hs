module LordOfTheRings.Ally.Cards.BilboBagginsTheRoadDarkens (bilboBagginsTheRoadDarkens) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (AttachmentType))
import LordOfTheRings.Trait (Trait (Pipe))

newtype BilboBagginsTheRoadDarkens = BilboBagginsTheRoadDarkens AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bilboBagginsTheRoadDarkens :: AllyCard BilboBagginsTheRoadDarkens
bilboBagginsTheRoadDarkens = ally BilboBagginsTheRoadDarkens Cards.bilboBagginsTheRoadDarkens

-- | Response: After Bilbo Baggins enters play, search your deck for a Pipe attachment and
-- add it to your hand. Shuffle your deck.
instance HasAbilities BilboBagginsTheRoadDarkens where
  getAbilities (BilboBagginsTheRoadDarkens a) = [mkAbility a 1 $ response $ entersPlay a]

instance RunMessage BilboBagginsTheRoadDarkens where
  runMessage msg s@(BilboBagginsTheRoadDarkens a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchDeck pid a maxBound (CardWithType AttachmentType <> CardWithTrait Pipe) False
      pure s
    _ -> BilboBagginsTheRoadDarkens <$> runMessage msg a
