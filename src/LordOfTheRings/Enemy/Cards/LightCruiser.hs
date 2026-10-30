module LordOfTheRings.Enemy.Cards.LightCruiser (lightCruiser) where

import LordOfTheRings.Card (cdCardTraits, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Raider))

newtype LightCruiser = LightCruiser EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

lightCruiser :: EnemyCard LightCruiser
lightCruiser = enemy LightCruiser Cards.lightCruiser

-- | Boarding 2. Cannot have attachments.
instance HasModifiersFor LightCruiser where
  getModifiersFor (LightCruiser a) = modified a a [CannotHaveAttachments]

-- | Each Raider enemy revealed from the Corsair deck by Light Cruiser's boarding keyword
-- enters play with 1 resource on it.
instance RunMessage LightCruiser where
  runMessage msg e@(LightCruiser attrs) = case msg of
    RevealedFromCorsairDeck (isSource attrs -> True) _ card | Raider `member` cdCardTraits (toCardDef card) -> do
      placeResources attrs (CardIdTarget card.id) 1
      pure e
    _ -> LightCruiser <$> runMessage msg attrs
