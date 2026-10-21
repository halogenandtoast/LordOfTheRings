module LordOfTheRings.Location.Cards.TheOldFord (theOldFord) where

import LordOfTheRings.Card (CardCost (StaticCost), CardDef (..), toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Riverland))

newtype TheOldFord = TheOldFord LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theOldFord :: LocationCard TheOldFord
theOldFord = location TheOldFord Cards.theOldFord

-- | X is the number of ally cards in play.
instance HasModifiersFor TheOldFord where
  getModifiersFor (TheOldFord a) = do
    allies <- selectCount AnyAlly
    modified a a [ThreatModifier allies]

-- | Shadow: Discard from play all allies with a printed cost lower than the number of
-- Riverland locations in play.
instance RunMessage TheOldFord where
  runMessage msg l@(TheOldFord a) = case msg of
    ResolveShadowEffect cid _ _ _ | cid == a.cardId -> do
      riverlands <- selectCount (LocationWithTrait Riverland)
      allies <- traverse getAlly =<< select AnyAlly
      for_ allies \x -> case cdCost (toCardDef x) of
        Just (StaticCost n) | n < riverlands -> discard (toTarget x)
        _ -> pure ()
      pure l
    _ -> TheOldFord <$> runMessage msg a
