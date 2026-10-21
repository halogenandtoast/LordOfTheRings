module LordOfTheRings.Treachery.Cards.ImpassableChasm (impassableChasm) where

import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ImpassableChasm = ImpassableChasm TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

impassableChasm :: TreacheryCard ImpassableChasm
impassableChasm = treachery ImpassableChasm Cards.impassableChasm

-- | When Revealed: If there is an active location, remove all progress tokens from that
-- location and return it to the staging area. If no location is moved by this effect,
-- this card gains surge.
instance RunMessage ImpassableChasm where
  runMessage msg t@(ImpassableChasm attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      active <- traverse getLocation =<< select IsActiveLocation
      when (null active) revealEncounterCard
      for_ active \l -> do
        when (l.progress > 0) $ removeTokens attrs l Progress l.progress
        placeLocation l.id StagingArea
      pure t
    _ -> ImpassableChasm <$> runMessage msg attrs
