module LordOfTheRings.Ally.Cards.CautiousHalfling (cautiousHalfling) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCardDef)

newtype CautiousHalfling = CautiousHalfling AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cautiousHalfling :: AllyCard CautiousHalfling
cautiousHalfling = ally CautiousHalfling Cards.cautiousHalfling

-- | Response: After Cautious Halfling enters play, look at the top card of the encounter
-- deck. If that card is not an enemy, draw a card.
instance HasAbilities CautiousHalfling where
  getAbilities (CautiousHalfling a) = [mkAbility a 1 $ response $ entersPlay a]

instance RunMessage CautiousHalfling where
  runMessage msg s@(CautiousHalfling a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      top <- getTopOfEncounterDeck
      lookAtTopOfEncounterDeck pid a False
      for_ top \c -> when (cdCardType (toCardDef c) /= EnemyType) $ drawCards a pid 1
      pure s
    _ -> CautiousHalfling <$> runMessage msg a
