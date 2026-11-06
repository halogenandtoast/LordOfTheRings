module LordOfTheRings.Enemy.Cards.SnagaScouts (snagaScouts) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Phase (Phase (EncounterPhase))

newtype SnagaScouts = SnagaScouts EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

snagaScouts :: EnemyCard SnagaScouts
snagaScouts = enemy SnagaScouts Cards.snagaScouts

instance HasAbilities SnagaScouts where
  getAbilities (SnagaScouts a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase]

instance RunMessage SnagaScouts where
  runMessage msg e@(SnagaScouts attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      players <- getRemainingPlayers
      threats <- traverse getThreat players
      lead <- getLead
      let lowest = [pid | (pid, threat) <- zip players threats, Just threat == minimumMay threats]
      chooseOrRunOneM lead do
        questionLabeled "snagaScouts.engage"
        causedBy attrs
        targets lowest \pid -> do
          scouts <- select $ EnemyWithCardCode (toCardCode attrs) <> NotEnemy (EnemyEngagedWith $ PlayerWithId pid)
          for_ scouts (engageEnemy pid)
      pure e
    _ -> SnagaScouts <$> runMessage msg attrs
