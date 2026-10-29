module LordOfTheRings.Event.Cards.ShadowsGiveWay (shadowsGiveWay) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype ShadowsGiveWay = ShadowsGiveWay EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | You must use resources from 3 different heroes' pools to pay for this card, on its card
-- def. Action: Discard each shadow card from each enemy in play.
shadowsGiveWay :: EventCard ShadowsGiveWay
shadowsGiveWay = event ShadowsGiveWay Cards.shadowsGiveWay

instance RunMessage ShadowsGiveWay where
  runMessage msg e@(ShadowsGiveWay attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      enemies <- traverse getEnemy =<< select AnyEnemy
      for_ enemies \enemy -> for_ enemy.shadowCards (discardShadowCard enemy.id)
      pure e
    _ -> ShadowsGiveWay <$> runMessage msg attrs
