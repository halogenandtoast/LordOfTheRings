module LordOfTheRings.Treachery.Cards.TownBaiting (townBaiting) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Helpers (lakeTownObjectives, smaugEnemies)

newtype TownBaiting = TownBaiting TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

townBaiting :: TreacheryCard TownBaiting
townBaiting = treachery TownBaiting Cards.townBaiting

instance HasModifiersFor TownBaiting where
  getModifiersFor (TownBaiting a) = modifySelf a [WhenRevealedCannotBeCanceled]

instance RunMessage TownBaiting where
  runMessage msg t@(TownBaiting a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      lastPlayer <- getLastPlayer
      towns <- lakeTownObjectives
      enemies <- smaugEnemies
      for_ towns \oid -> for_ enemies \eid ->
        immediateAttackWithPreparation 1 lastPlayer eid do
          -- Sentinel applies to attacks against other players, not this objective.
          enemyAttackModifier a eid (DefendersControlledBy lastPlayer)
          enemyAttackModifier a eid (UndefendedDamageTo $ ObjectiveTarget oid)
          enemyAttackModifier a eid (ExcessDamageTo $ ObjectiveTarget oid)
      pure t
    _ -> TownBaiting <$> runMessage msg a
