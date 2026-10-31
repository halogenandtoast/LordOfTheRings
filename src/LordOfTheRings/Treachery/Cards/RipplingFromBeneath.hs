module LordOfTheRings.Treachery.Cards.RipplingFromBeneath (ripplingFromBeneath) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Scenario.Scenarios.TheWatcherInTheWater.Helpers (isTentacleEnemy)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RipplingFromBeneath = RipplingFromBeneath TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ripplingFromBeneath :: TreacheryCard RipplingFromBeneath
ripplingFromBeneath = treachery RipplingFromBeneath Cards.ripplingFromBeneath

-- | Surge.
-- When Revealed: Shuffle X Tentacle enemies from the encounter discard pile into the
-- encounter deck. Heal X damage from The Watcher, if it is in play. X is the number of
-- players in the game.
instance RunMessage RipplingFromBeneath where
  runMessage msg t@(RipplingFromBeneath attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- length <$> getRemainingPlayers
      tentacles <- take x . filter isTentacleEnemy <$> getEncounterDiscard
      -- out of the discard pile, then shuffled in as set-aside cards are
      for_ tentacles \c -> removeFromEncounterDiscard c >> setAside c
      shuffleSetAsideIntoEncounterDeck tentacles
      watchers <- select (EnemyWithCardCode $ toCardCode Enemies.theWatcherNightmare)
      for_ watchers \w -> healDamage attrs w x
      pure t
    _ -> RipplingFromBeneath <$> runMessage msg attrs
