module LordOfTheRings.Hero.Cards.EomerLeadership (eomerLeadership) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype EomerLeadership = EomerLeadership HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eomerLeadership :: HeroCard EomerLeadership
eomerLeadership = hero EomerLeadership Cards.eomerLeadership

-- | Response: After Éomer commits to the quest, spend 1 resource from his resource pool to
-- immediately declare him as an attacker (and resolve his attack) against an enemy in the
-- staging area.
instance HasAbilities EomerLeadership where
  getAbilities (EomerLeadership a) =
    [ restricted a 1 (exists $ EnemyInStagingArea <> EnemyNotImmuneToPlayerCards)
        $ ResponseAbility (CommittedToQuestWindow After (toTarget a)) (ResourceCost (toTarget a) 1)
    ]

instance RunMessage EomerLeadership where
  runMessage msg h@(EomerLeadership attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      enemies <- select (EnemyInStagingArea <> EnemyNotImmuneToPlayerCards)
      chooseOrRunOneM pid do
        questionLabeled "eomer.chooseEnemy"
        causedBy attrs
        targets enemies \eid -> attackEnemyWith pid attrs (toTarget attrs) eid 0
      pure h
    _ -> EomerLeadership <$> runMessage msg attrs
