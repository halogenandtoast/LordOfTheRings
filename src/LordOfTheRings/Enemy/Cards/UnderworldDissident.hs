module LordOfTheRings.Enemy.Cards.UnderworldDissident (underworldDissident) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (removeTokenFromQuest)
import LordOfTheRings.Trait (Trait (Underworld))

newtype UnderworldDissident = UnderworldDissident EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

underworldDissident :: EnemyCard UnderworldDissident
underworldDissident = enemy UnderworldDissident Cards.underworldDissident

-- | Underworld Dissident gets +1 attack for each Underworld location in play.
instance HasModifiersFor UnderworldDissident where
  getModifiersFor (UnderworldDissident a) = do
    n <- length <$> select (LocationWithTrait Underworld)
    when (n > 0) $ modified a a [AttackModifier n]

-- | Shadow: If this attack is undefended, remove 1 token from the current quest.
instance RunMessage UnderworldDissident where
  runMessage msg e@(UnderworldDissident attrs) = case msg of
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      when (null defenders) $ removeTokenFromQuest eid
      pure e
    _ -> UnderworldDissident <$> runMessage msg attrs
