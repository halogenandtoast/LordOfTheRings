module LordOfTheRings.Event.Cards.HornsHornsHorns (hornsHornsHorns) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype HornsHornsHorns = HornsHornsHorns EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Doomed 2, on its card def. Action: Each player may put one ally card into play from his
-- hand. At the end of the phase, shuffle each of those allies that are still in play into
-- their owners' decks.
hornsHornsHorns :: EventCard HornsHornsHorns
hornsHornsHorns = event HornsHornsHorns Cards.hornsHornsHorns

instance RunMessage HornsHornsHorns where
  runMessage msg e@(HornsHornsHorns attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      eachPlayer \p -> do
        allies <- filterM passesUnique . filter ((== AllyType) . (.cardType)) . (.hand) =<< getPlayer p
        chooseOneM p do
          questionLabeled "hornsHornsHorns.chooseAlly"
          causedBy attrs
          targets allies \c -> do
            putAllyIntoPlayFromHand p c.id
            phaseModifier attrs (CardIdTarget c.id) ShuffleIntoDeckAtEndOfPhase
          labeled "hornsHornsHorns.none" $ pure ()
      pure e
    _ -> HornsHornsHorns <$> runMessage msg attrs
