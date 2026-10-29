module LordOfTheRings.Location.Cards.OrcCave (orcCave) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (searches)
import LordOfTheRings.Trait (Trait (Orc))

newtype OrcCave = OrcCave LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcCave :: LocationCard OrcCave
orcCave = location OrcCave Cards.orcCave

-- | Forced: After Orc Cave leaves play as an explored location, the first player Searches 5.
instance HasAbilities OrcCave where
  getAbilities (OrcCave a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

-- | Travel: Discard the top X cards of the encounter deck to travel here. X is the number
-- of players in the game. Add each Orc enemy discarded this way to the staging area.
instance RunMessage OrcCave where
  runMessage msg l@(OrcCave a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      searches lead 5
      pure l
    TravelTo lid | lid == a.id -> do
      players <- getRemainingPlayers
      discardTopOfEncounterDeck a (length players)
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ (filter (cardMatches $ CardWithType EnemyType <> CardWithTrait Orc) cards) addToStagingFromDiscard
      pure l
    _ -> OrcCave <$> runMessage msg a
