module LordOfTheRings.Event.Cards.GrimResolve (grimResolve) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype GrimResolve = GrimResolve EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Ready all characters in play.
grimResolve :: EventCard GrimResolve
grimResolve = event GrimResolve Cards.grimResolve

instance RunMessage GrimResolve where
  runMessage msg e@(GrimResolve attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      pushAll . map Ready =<< selectCharacters ExhaustedHero ExhaustedAlly
      pure e
    _ -> GrimResolve <$> runMessage msg attrs
