module LordOfTheRings.Enemy.Cards.VanguardOfBolg (vanguardOfBolg) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (stagesWithoutProgress)

newtype VanguardOfBolg = VanguardOfBolg EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

vanguardOfBolg :: EnemyCard VanguardOfBolg
vanguardOfBolg = enemy VanguardOfBolg Cards.vanguardOfBolg

-- | This enemy gets +1 threat, +1 attack, and +1 defense for each quest stage in play with
-- no progress on it.
instance HasModifiersFor VanguardOfBolg where
  getModifiersFor (VanguardOfBolg a) = do
    n <- stagesWithoutProgress
    when (n > 0) $ modifySelf a [ThreatModifier n, AttackModifier n, DefenseModifier n]

-- | Shadow: If this attack destroys a character, remove all progress from the current
-- quest.
instance RunMessage VanguardOfBolg where
  runMessage msg e@(VanguardOfBolg attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      current <- getCurrentQuest
      progress <- getsGame (sum . map (.progress) . filter ((== current) . Just . toTarget) . toList . (.quests) . (.entities))
      when (progress > 0) $ removeProgressFromQuest (CardIdSource cid) progress
      pure e
    _ -> VanguardOfBolg <$> runMessage msg attrs
