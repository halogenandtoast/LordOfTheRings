module LordOfTheRings.Ally.Cards.Eomund (eomund) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Rohan))

newtype Eomund = Eomund AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eomund :: AllyCard Eomund
eomund = ally Eomund Cards.eomund

-- | Response: After Éomund leaves play, ready all Rohan characters in play.
instance HasAbilities Eomund where
  getAbilities (Eomund a) = [mkAbility a 1 $ response $ LeftPlayWindow After (toTarget a)]

instance RunMessage Eomund where
  runMessage msg s@(Eomund a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      rohan <- selectCharacters (HeroWithTrait Rohan) (AllyWithTrait Rohan)
      for_ rohan ready
      pure s
    _ -> Eomund <$> runMessage msg a
