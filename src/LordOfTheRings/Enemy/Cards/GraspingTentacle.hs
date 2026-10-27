module LordOfTheRings.Enemy.Cards.GraspingTentacle (graspingTentacle) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheWatcherInTheWater.Helpers (tentacleStrikes)

newtype GraspingTentacle = GraspingTentacle EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

graspingTentacle :: EnemyCard GraspingTentacle
graspingTentacle = enemy GraspingTentacle Cards.graspingTentacle

-- | As a Tentacle attachment: "Attached character's attack and defense are reduced to 0."
instance HasModifiersFor GraspingTentacle where
  getModifiersFor (GraspingTentacle a) = withHost a \host -> modified a host [DoesNotCountAttack, DoesNotCountDefense]

-- | Forced: When Grasping Tentacle is attacked, discard the top card of the encounter deck.
-- If that card has a shadow effect or is a Tentacle enemy, attach this card to an
-- attacking character as a Tentacle attachment. The attack then does nothing: there is no
-- enemy left to deal its damage to.
instance HasAbilities GraspingTentacle where
  getAbilities (GraspingTentacle a) = [mkAbility a 1 $ forced $ EnemyAttackedWindow When a.id]

instance RunMessage GraspingTentacle where
  runMessage msg e@(GraspingTentacle attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs 1
      pure e
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards | tentacleStrikes cards -> do
      attackers <- selectCharacters AttackingHero AttackingAlly
      lead <- getLead
      chooser <- fromMaybe lead . join <$> traverse characterController (headMay attackers)
      chooseOrRunOneM chooser do
        questionLabeled "graspingTentacle.attach"
        causedBy attrs
        targets attackers \c -> placeEnemy attrs.id (AttachedTo c)
      pure e
    _ -> GraspingTentacle <$> runMessage msg attrs
