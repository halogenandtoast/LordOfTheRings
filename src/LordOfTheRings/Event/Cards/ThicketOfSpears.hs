module LordOfTheRings.Event.Cards.ThicketOfSpears (thicketOfSpears) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype ThicketOfSpears = ThicketOfSpears EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | You must use resources from 3 different heroes' pools to pay for this card.
-- Action: Choose a player. That player's engaged enemies cannot attack that player this
-- phase.
thicketOfSpears :: EventCard ThicketOfSpears
thicketOfSpears = event ThicketOfSpears Cards.thicketOfSpears

instance RunMessage ThicketOfSpears where
  runMessage msg e@(ThicketOfSpears attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      push =<< choosePlayer pid (Just $ toTarget attrs) \p ->
        [CreateEffect $ Effect (toSource attrs) (PlayerTarget p) [EngagedEnemiesCannotAttack] UntilEndOfPhase]
      pure e
    _ -> ThicketOfSpears <$> runMessage msg attrs
