module LordOfTheRings.Event.Cards.EverOnward (everOnward) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype EverOnward = EverOnward EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Response: After players quest unsuccessfully, choose a player. That player does not
-- raise his threat. Played in the window before the threat is raised.
everOnward :: EventCard EverOnward
everOnward = eventWith EverOnward Cards.everOnward (windowL ?~ QuestedUnsuccessfullyWindow When)

-- | The chosen player comes back as a later step, carried as the event's player, so the
-- raise is taken off the game's queue once the choice is made.
instance RunMessage EverOnward where
  runMessage msg e@(EverOnward attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      choosePlayerM pid attrs \p -> doStep 1 (PlayEvent p card windows)
      pure e
    DoStep 1 (PlayEvent p card _) | card.id == toId attrs -> do
      doesNotRaiseThreat p
      pure e
    _ -> EverOnward <$> runMessage msg attrs
