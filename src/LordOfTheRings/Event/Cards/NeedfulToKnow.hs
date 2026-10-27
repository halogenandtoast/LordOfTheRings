module LordOfTheRings.Event.Cards.NeedfulToKnow (needfulToKnow) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Card (cdThreat, toCardDef)

newtype NeedfulToKnow = NeedfulToKnow EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Secrecy 2, on its card def.
-- Action: Raise your threat by 1 to look at the top card of the encounter deck. Then,
-- reduce your threat by X, where X is the threat of that card.
needfulToKnow :: EventCard NeedfulToKnow
needfulToKnow = event NeedfulToKnow Cards.needfulToKnow

instance RunMessage NeedfulToKnow where
  runMessage msg e@(NeedfulToKnow attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      raiseThreat pid 1
      lookAtTopOfEncounterDeck pid attrs False
      top <- getTopOfEncounterDeck
      for_ (cdThreat . toCardDef =<< top) \x -> when (x > 0) $ lowerThreat pid x
      pure e
    _ -> NeedfulToKnow <$> runMessage msg attrs
