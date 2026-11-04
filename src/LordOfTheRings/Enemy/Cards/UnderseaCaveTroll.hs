module LordOfTheRings.Enemy.Cards.UnderseaCaveTroll (underseaCaveTroll) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Character (getDefenseAgainst, getHitPoints)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype UnderseaCaveTroll = UnderseaCaveTroll EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

underseaCaveTroll :: EnemyCard UnderseaCaveTroll
underseaCaveTroll = enemy UnderseaCaveTroll Cards.underseaCaveTroll

-- | The damage its attack deals the defender beyond its remaining hit points.
excessOn :: HasGame m => EnemyId -> Target -> m Int
excessOn eid defender = do
  attack <- getEnemyAttack eid
  defense <- getDefenseAgainst eid defender
  remaining <- (-) <$> getHitPoints defender <*> getDamage defender
  pure $ max 0 (attack - defense - remaining)

-- | For each point of excess damage dealt by Undersea Cave Troll (damage that is dealt
-- beyond the remaining hit points of the character damaged by this attack) discard 1
-- resource from one of your objective-allies or from one of your heroes' resource pools.
-- The excess is read as the attack's damage is dealt to the defender.
instance RunMessage UnderseaCaveTroll where
  runMessage msg e@(UnderseaCaveTroll attrs) = case msg of
    AttackDamage eid defender | eid == attrs.id, character defender, EngagedWith _ <- attrs.placement -> do
      excess <- excessOn eid defender
      doStep excess msg
      UnderseaCaveTroll <$> runMessage msg attrs
    -- one resource at a time, the player choosing where each comes from
    DoStep n inner@(AttackDamage eid _) | eid == attrs.id, n > 0, EngagedWith pid <- attrs.placement -> do
      heroes <- select (HeroControlledBy (PlayerWithId pid) <> HeroWithResources)
      allies <- filterM (fmap ((> 0) . (.resources)) . getAlly) =<< select (AllyControlledBy (PlayerWithId pid) <> ObjectiveAlly)
      chooseOrRunOneM pid do
        questionLabeled "discardResourceFromAHero"
        causedBy attrs
        targets (map toTarget heroes <> map toTarget allies) \t -> removeTokens attrs t Resource 1
      doStep (n - 1) inner
      pure e
    _ -> UnderseaCaveTroll <$> runMessage msg attrs
   where
    character = \case
      HeroTarget _ -> True
      AllyTarget _ -> True
      _ -> False
