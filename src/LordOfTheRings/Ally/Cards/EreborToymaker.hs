module LordOfTheRings.Ally.Cards.EreborToymaker (ereborToymaker) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (AttachmentType))

newtype EreborToymaker = EreborToymaker AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ereborToymaker :: AllyCard EreborToymaker
ereborToymaker = ally EreborToymaker Cards.ereborToymaker

-- | Response: After you play Erebor Toymaker from your hand, reduce the cost of the next
-- attachment played this phase by 2.
instance HasAbilities EreborToymaker where
  getAbilities (EreborToymaker a) = [mkAbility a 1 $ response $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)]

instance RunMessage EreborToymaker where
  runMessage msg s@(EreborToymaker a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      phaseModifier a GameTarget (LowerNextCostOf (CardWithType AttachmentType) 2)
      pure s
    _ -> EreborToymaker <$> runMessage msg a
