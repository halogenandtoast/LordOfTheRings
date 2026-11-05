module LordOfTheRings.Enemy.Cards.GoblinProwler (goblinProwler) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.OverTheMistyMountainsGrim.Helpers (greatGoblinDefeated)

newtype GoblinProwler = GoblinProwler EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinProwler :: EnemyCard GoblinProwler
goblinProwler = enemy GoblinProwler Cards.goblinProwler

instance HasModifiersFor GoblinProwler where
  getModifiersFor (GoblinProwler a) = whenM greatGoblinDefeated $ modifySelf a [EngagementCostModifier (-30)]

instance HasAbilities GoblinProwler where
  getAbilities (GoblinProwler a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage GoblinProwler where
  runMessage msg e@(GoblinProwler attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ | EngagedWith pid <- attrs.placement -> do
      characters <- charactersControlledBy pid
      chooseOrRunOneM pid do
        questionLabeled "goblinProwler.discard"
        causedBy attrs
        targets characters (discard attrs)
      pure e
    _ -> GoblinProwler <$> runMessage msg attrs
