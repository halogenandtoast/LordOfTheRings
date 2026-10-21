module LordOfTheRings.Ally.Cards.WestfoldHorseBreeder (westfoldHorseBreeder) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (AttachmentType))
import LordOfTheRings.Trait (Trait (Mount))

newtype WestfoldHorseBreeder = WestfoldHorseBreeder AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

westfoldHorseBreeder :: AllyCard WestfoldHorseBreeder
westfoldHorseBreeder = ally WestfoldHorseBreeder Cards.westfoldHorseBreeder

-- | Response: After Westfold Horse-breeder enters play, search the top 10 cards of your
-- deck for a Mount attachment and add it to your hand. Shuffle your deck.
instance HasAbilities WestfoldHorseBreeder where
  getAbilities (WestfoldHorseBreeder a) = [mkAbility a 1 $ response $ EntersPlayWindow After (toTarget a)]

instance RunMessage WestfoldHorseBreeder where
  runMessage msg s@(WestfoldHorseBreeder a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      searchDeck pid a 10 (CardWithTrait Mount <> CardWithType AttachmentType) False
      pure s
    _ -> WestfoldHorseBreeder <$> runMessage msg a
