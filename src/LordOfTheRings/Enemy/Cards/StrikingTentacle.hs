module LordOfTheRings.Enemy.Cards.StrikingTentacle (strikingTentacle) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWatcherInTheWater.Helpers (tentacleStrikes)

newtype StrikingTentacle = StrikingTentacle EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

strikingTentacle :: EnemyCard StrikingTentacle
strikingTentacle = enemy StrikingTentacle Cards.strikingTentacle

-- | Forced: When Striking Tentacle attacks, discard the top card of the encounter deck. If
-- that card has a shadow effect or is a Tentacle enemy, this attack is considered
-- undefended.
instance HasAbilities StrikingTentacle where
  getAbilities (StrikingTentacle a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage StrikingTentacle where
  runMessage msg e@(StrikingTentacle attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs 1
      pure e
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards | tentacleStrikes cards -> do
      attackModifier attrs attrs CannotBeDefended
      pure e
    _ -> StrikingTentacle <$> runMessage msg attrs
