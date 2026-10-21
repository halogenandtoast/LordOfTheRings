module LordOfTheRings.Enemy.Cards.Louis (louis) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Troll))

newtype Louis = Louis EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

louis :: EnemyCard Louis
louis = enemy Louis Cards.louis

-- | While Louis is engaged with a player, all Troll enemies gain, "Forced: After this
-- enemy attacks, the defending player must raise his threat by 3."
-- (The response after defeating him is the scenario's.)
instance HasAbilities Louis where
  getAbilities (Louis a) = [restricted a 1 (exists $ EnemyWithId a.id <> EnemyEngagedWith AnyPlayer) $ forced $ AnyEnemyAttacksWindow After]

instance RunMessage Louis where
  runMessage msg e@(Louis attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [(eid, pid) | Window _ (EnemyAttacking eid pid) <- windows] \(eid, pid) -> do
        troll <- maybe False (member Troll . cdCardTraits . toCardDef) <$> selectEnemy eid
        when troll $ raiseThreat pid 3
      pure e
    _ -> Louis <$> runMessage msg attrs
   where
    selectEnemy eid = do
      present <- selectAny (EnemyWithId eid)
      if present then Just <$> getEnemy eid else pure Nothing
