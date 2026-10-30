module LordOfTheRings.Location.Cards.GhostlyRuins (ghostlyRuins) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (undeadEnemy)

newtype GhostlyRuins = GhostlyRuins LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ghostlyRuins :: LocationCard GhostlyRuins
ghostlyRuins = location GhostlyRuins Cards.ghostlyRuins

-- | Any time the encounter deck runs out of cards, return the topmost Undead enemy in the
-- encounter discard pile to the staging area before resetting the encounter deck.
instance HasModifiersFor GhostlyRuins where
  getModifiersFor (GhostlyRuins a) = modified a GameTarget [ReturnBeforeEncounterDeckResets undeadEnemy]

-- | Travel: Return the topmost Undead enemy in the encounter discard pile to the staging
-- area.
instance RunMessage GhostlyRuins where
  runMessage msg l@(GhostlyRuins a) = case msg of
    TravelTo lid | lid == a.id -> do
      pile <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      traverse_ addToStagingFromDiscard $ take 1 $ filter (cardMatches undeadEnemy) pile
      pure l
    _ -> GhostlyRuins <$> runMessage msg a
