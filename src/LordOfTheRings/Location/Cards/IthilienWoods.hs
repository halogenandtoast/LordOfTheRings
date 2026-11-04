module LordOfTheRings.Location.Cards.IthilienWoods (ithilienWoods) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (haradEnemy)

newtype IthilienWoods = IthilienWoods LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ithilienWoods :: LocationCard IthilienWoods
ithilienWoods = location IthilienWoods Cards.ithilienWoods

-- | While Ithilien Woods is the active location, it gains: "Forced: After a player engages
-- a Harad enemy, he readies a character he controls." Engaging an enemy and being engaged
-- by it are the same (FAQ 1.49).
instance HasAbilities IthilienWoods where
  getAbilities (IthilienWoods a) = [mkAbility a 1 $ forced $ MatchingEnemyEngagedWindow After haradEnemy | a.placement == ActiveLocation]

-- | Travel: The first player reveals the top card of the encounter deck to travel here.
instance RunMessage IthilienWoods where
  runMessage msg l@(IthilienWoods a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      revealEncounterCardAs lead
      IthilienWoods <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        let yours = PlayerWithId pid
        exhausted <- selectCharacters (HeroControlledBy yours <> ExhaustedHero) (AllyControlledBy yours <> ExhaustedAlly)
        chooseOrRunOneM pid do
          questionLabeled "ithilienWoods.ready"
          causedBy a
          targets exhausted ready
      pure l
    _ -> IthilienWoods <$> runMessage msg a
