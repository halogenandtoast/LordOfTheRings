module LordOfTheRings.Enemy.Cards.ThrashingTentacle (thrashingTentacle) where

import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Character (getAttackStrength)
import LordOfTheRings.Helpers.Staging (getEnemyDefense)
import LordOfTheRings.Scenario.Scenarios.TheWatcherInTheWater.Helpers (tentacleStrikes)

newtype ThrashingTentacle = ThrashingTentacle EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thrashingTentacle :: EnemyCard ThrashingTentacle
thrashingTentacle = enemy ThrashingTentacle Cards.thrashingTentacle

-- | Forced: When Thrashing Tentacle is attacked, discard the top card of the encounter
-- deck. If that card has a shadow effect or is a Tentacle enemy, deal the damage from the
-- attack to 1 character an attacking player controls (ignoring defense), in place of
-- Thrashing Tentacle.
instance HasAbilities ThrashingTentacle where
  getAbilities (ThrashingTentacle a) = [mkAbility a 1 $ forced $ EnemyAttackedWindow When a.id]

instance RunMessage ThrashingTentacle where
  runMessage msg e@(ThrashingTentacle attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs 1
      pure e
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards | tentacleStrikes cards -> do
      attackers <- selectCharacters AttackingHero AttackingAlly
      strength <- getAttackStrength attackers attrs.id (toCardDef attrs)
      damage <- max 0 . (strength -) <$> getEnemyDefense attrs.id
      players <- ordNub . catMaybes <$> traverse characterController attackers
      characters <- concatMapM charactersControlledBy players
      attackModifier attrs attrs CannotTakeCombatDamage
      -- the first attacking player chooses among the attacking players' characters
      for_ (headMay players) \p -> when (damage > 0) $ chooseOrRunOneM p do
        questionLabeledWith "thrashingTentacle.damage" [intVar "n" damage]
        causedBy attrs
        targets characters \c -> dealDamage attrs c damage
      pure e
    _ -> ThrashingTentacle <$> runMessage msg attrs
