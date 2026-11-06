module LordOfTheRings.Location.Cards.EsgarothWharf (esgarothWharf) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype EsgarothWharf = EsgarothWharf LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

esgarothWharf :: LocationCard EsgarothWharf
esgarothWharf = location EsgarothWharf Cards.esgarothWharf

instance HasModifiersFor EsgarothWharf where
  getModifiersFor (EsgarothWharf a) = do
    locations <- select AnyLocation
    for_ locations \lid -> modified a lid [ImmuneToPlayerCardEffects]

instance RunMessage EsgarothWharf where
  runMessage msg l@(EsgarothWharf a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      attackModifier (CardIdSource cid) eid (AttackModifier 2)
      additionalAttack pid eid
      pure l
    _ -> EsgarothWharf <$> runMessage msg a
