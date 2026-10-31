module LordOfTheRings.Enemy.Cards.PackHunters (packHunters) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (isNight, wargEnemy)

newtype PackHunters = PackHunters EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

packHunters :: EnemyCard PackHunters
packHunters = enemy PackHunters Cards.packHunters

-- | While it is Night, Pack Hunters gets -30 engagement cost.
instance HasModifiersFor PackHunters where
  getModifiersFor (PackHunters a) = whenM isNight $ modified a a [EngagementCostModifier (-30)]

-- | Forced: When you engage Pack Hunters, search the top 5 cards of the encounter deck for a
-- Warg enemy, and put it into play engaged with you (top 10 cards instead if it is Night).
-- Shuffle the encounter deck.
instance HasAbilities PackHunters where
  getAbilities (PackHunters a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage PackHunters where
  runMessage msg e@(PackHunters attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      n <- bool 5 10 <$> isNight
      top <- getsGame (take n . (.encounterDeck) . toAttrs . (.scenario))
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        chooseOrRunOneM pid do
          questionLabeled "packHunters.search"
          causedBy attrs
          targets (filter (cardMatches wargEnemy) top) \c -> do
            removeFromEncounterDeck c
            putEnemyIntoPlayEngaged pid c
      shuffleEncounterDeck
      pure e
    _ -> PackHunters <$> runMessage msg attrs
