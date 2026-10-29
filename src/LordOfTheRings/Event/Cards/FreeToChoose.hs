module LordOfTheRings.Event.Cards.FreeToChoose (freeToChoose) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype FreeToChoose = FreeToChoose EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After your threat is raised by an encounter card effect, reduce your threat
-- by an equal amount.
freeToChoose :: EventCard FreeToChoose
freeToChoose =
  eventWith FreeToChoose Cards.freeToChoose \a -> a & windowL ?~ ThreatRaisedByEncounterWindow After a.owner

instance RunMessage FreeToChoose where
  runMessage msg e@(FreeToChoose attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      for_ (take 1 [n | Window _ (ThreatRaisedByEncounter p n) <- windows, p == pid]) (lowerThreatBy attrs pid)
      pure e
    _ -> FreeToChoose <$> runMessage msg attrs
