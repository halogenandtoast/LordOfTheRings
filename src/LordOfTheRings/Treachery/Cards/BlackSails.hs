module LordOfTheRings.Treachery.Cards.BlackSails (blackSails) where

import LordOfTheRings.Scenario.Scenarios.TheSiegeOfGondor.Helpers (shipLocation)
import LordOfTheRings.Trait (Trait (Ship))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BlackSails = BlackSails TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 1.
blackSails :: TreacheryCard BlackSails
blackSails = treachery BlackSails Cards.blackSails

-- | When Revealed: Each Ship location in the staging area gets +1 threat until the end of
-- the phase. If there are no non-unique Ship locations in the staging area, search the
-- encounter deck and discard pile for a Ship location and add it to the staging area.
-- Shuffle the encounter deck. The revealer searches.
instance RunMessage BlackSails where
  runMessage msg t@(BlackSails attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      ships <- select (LocationInStagingArea <> LocationWithTrait Ship)
      for_ ships \l -> phaseModifier attrs l (ThreatModifier 1)
      nonUnique <- selectAny (LocationInStagingArea <> LocationWithTrait Ship <> not_ UniqueLocation)
      if nonUnique
        then shuffleEncounterDeck
        else do
          revealer <- getRevealer
          searchEncounterDeckAndDiscard revealer attrs shipLocation
      pure t
    _ -> BlackSails <$> runMessage msg attrs
