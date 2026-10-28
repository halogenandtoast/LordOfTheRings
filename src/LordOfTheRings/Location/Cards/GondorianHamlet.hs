module LordOfTheRings.Location.Cards.GondorianHamlet (gondorianHamlet) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (discardVillagers, withVillagers)

newtype GondorianHamlet = GondorianHamlet LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Villagers 3.
gondorianHamlet :: LocationCard GondorianHamlet
gondorianHamlet = location (GondorianHamlet . withVillagers 3) Cards.gondorianHamlet

-- | While Gondorian Hamlet is in the staging area it gains: "Forced: After a treachery card
-- is revealed from the encounter deck, discard 1 villager token from Gondorian Hamlet."
instance HasAbilities GondorianHamlet where
  getAbilities (GondorianHamlet a) =
    [mkAbility a 1 $ forced $ TreacheryRevealedWindow After | a.placement == StagingArea]

instance RunMessage GondorianHamlet where
  runMessage msg l@(GondorianHamlet a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      void $ discardVillagers a a 1
      pure l
    _ -> GondorianHamlet <$> runMessage msg a
