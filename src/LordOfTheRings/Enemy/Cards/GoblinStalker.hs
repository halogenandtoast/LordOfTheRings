module LordOfTheRings.Enemy.Cards.GoblinStalker (goblinStalker) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (passesLocateTest)

newtype GoblinStalker = GoblinStalker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinStalker :: EnemyCard GoblinStalker
goblinStalker = enemy GoblinStalker Cards.goblinStalker

-- | Forced: When Goblin Stalker attacks, deal it 1 additional shadow card.
instance HasAbilities GoblinStalker where
  getAbilities (GoblinStalker a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Forced: When Goblin Stalker is dealt a shadow card with 'PASS' printed in its text
-- box, it gets +2 [Attack] until the end of the phase.
instance RunMessage GoblinStalker where
  runMessage msg e@(GoblinStalker attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      dealShadowCard attrs.id
      pure e
    CheckWindows ws -> do
      let dealt = [cid | Window _ (ShadowCardDealt eid cid _) <- ws, eid == attrs.id]
          passing = [c | c <- attrs.shadowCards, c.id `elem` dealt, passesLocateTest c]
      for_ passing \_ -> phaseModifier attrs attrs (AttackModifier 2)
      GoblinStalker <$> runMessage msg attrs
    _ -> GoblinStalker <$> runMessage msg attrs
