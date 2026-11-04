module LordOfTheRings.Enemy.Cards.BodyguardOfBolg (bodyguardOfBolg) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (stagesWithoutProgress)

newtype BodyguardOfBolg = BodyguardOfBolg EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

bodyguardOfBolg :: EnemyCard BodyguardOfBolg
bodyguardOfBolg = enemy BodyguardOfBolg Cards.bodyguardOfBolg

-- | Bolg cannot take damage.
instance HasModifiersFor BodyguardOfBolg where
  getModifiersFor (BodyguardOfBolg a) = do
    bolgs <- select (EnemyWithCardCode $ toCardCode Cards.bolg)
    for_ bolgs \b -> modified a b [CannotTakeDamage]

-- | Forced: When this enemy attacks, deal it an additional shadow card for each quest stage
-- in play with no progress tokens on it.
instance HasAbilities BodyguardOfBolg where
  getAbilities (BodyguardOfBolg a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage BodyguardOfBolg where
  runMessage msg e@(BodyguardOfBolg attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      n <- stagesWithoutProgress
      replicateM_ n (dealShadowCard attrs.id)
      pure e
    _ -> BodyguardOfBolg <$> runMessage msg attrs
