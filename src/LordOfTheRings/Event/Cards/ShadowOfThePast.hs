module LordOfTheRings.Event.Cards.ShadowOfThePast (shadowOfThePast) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype ShadowOfThePast = ShadowOfThePast EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Move the top card of the encounter discard pile to the top of the encounter
-- deck.
shadowOfThePast :: EventCard ShadowOfThePast
shadowOfThePast = event ShadowOfThePast Cards.shadowOfThePast

instance RunMessage ShadowOfThePast where
  runMessage msg e@(ShadowOfThePast attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      top <- getsGame (headMay . (.encounterDiscard) . toAttrs . (.scenario))
      for_ top moveToTopOfEncounterDeck
      pure e
    _ -> ShadowOfThePast <$> runMessage msg attrs
