module LordOfTheRings.Treachery.Cards.WindWhippedRain (windWhippedRain) where

import LordOfTheRings.Card (CardDef (cdTreasure), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WindWhippedRain = WindWhippedRain TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

windWhippedRain :: TreacheryCard WindWhippedRain
windWhippedRain = treachery WindWhippedRain Cards.windWhippedRain

-- | When Revealed: Discard all non-treasure, non-objective attachments in play: player
-- attachments, and encounter cards attached as conditions.
-- Shadow: The defending character gets -1 defense.
instance RunMessage WindWhippedRain where
  runMessage msg t@(WindWhippedRain attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      es <- getsGame (.entities)
      let attachments = [toTarget a | a <- toList es.attachments, not (cdTreasure $ toCardDef a)]
          conditions = [toTarget c | c <- toList es.treacheries, isAttached c.placement]
      traverse_ (discard attrs) (attachments <> conditions)
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> attackModifier attrs d (DefenseModifier (-1))
      pure t
    _ -> WindWhippedRain <$> runMessage msg attrs
   where
    isAttached = \case
      AttachedTo _ -> True
      _ -> False
