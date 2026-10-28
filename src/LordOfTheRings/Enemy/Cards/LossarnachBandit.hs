module LordOfTheRings.Enemy.Cards.LossarnachBandit (lossarnachBandit) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.PerilInPelargir.Helpers (wasOptionallyEngaged)

newtype LossarnachBandit = LossarnachBandit EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lossarnachBandit :: EnemyCard LossarnachBandit
lossarnachBandit = enemy LossarnachBandit Cards.lossarnachBandit

-- | Forced: When Lossarnach Bandit engages a player, that player discards 1 resource from
-- each of his heroes' resource pools. (2 resources instead if Lossarnach Bandit was not
-- optionally engaged.)
instance HasAbilities LossarnachBandit where
  getAbilities (LossarnachBandit a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage LossarnachBandit where
  runMessage msg e@(LossarnachBandit attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        opted <- wasOptionallyEngaged attrs.id pid
        heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId pid)
        for_ heroes \h -> do
          let n = min h.resources (if opted then 1 else 2)
          when (n > 0) $ removeTokens attrs h Resource n
      pure e
    _ -> LossarnachBandit <$> runMessage msg attrs
