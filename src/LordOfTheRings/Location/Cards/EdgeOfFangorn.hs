module LordOfTheRings.Location.Cards.EdgeOfFangorn (edgeOfFangorn) where

import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.IntoFangorn.Helpers (huorn)

newtype EdgeOfFangorn = EdgeOfFangorn LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

edgeOfFangorn :: LocationCard EdgeOfFangorn
edgeOfFangorn = location EdgeOfFangorn Cards.edgeOfFangorn

-- | Immune to player card effects.
-- Travel: Search the encounter deck and discard pile for a Huorn enemy and add it to the
-- staging area to travel here (2 Huorn enemies instead if there are 3 or more players in
-- the game). Without as many to find, the cost cannot be paid.
instance HasModifiersFor EdgeOfFangorn where
  getModifiersFor (EdgeOfFangorn a) = do
    needed <- huornsNeeded
    found <- getsGame \g -> let s = toAttrs g.scenario in length $ filter (cardMatches huorn) (s.encounterDeck <> s.encounterDiscard)
    modified a a $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | found < needed]

instance RunMessage EdgeOfFangorn where
  runMessage msg l@(EdgeOfFangorn a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      needed <- huornsNeeded
      replicateM_ needed $ searchEncounterDeckAndDiscard lead a huorn
      pure l
    _ -> EdgeOfFangorn <$> runMessage msg a

huornsNeeded :: HasGame m => m Int
huornsNeeded = (\n -> if n >= 3 then 2 else 1) . length <$> getRemainingPlayers
