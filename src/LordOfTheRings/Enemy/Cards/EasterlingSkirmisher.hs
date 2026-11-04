module LordOfTheRings.Enemy.Cards.EasterlingSkirmisher (easterlingSkirmisher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.ControlledLocation (returnHighestThreatLocation)

newtype EasterlingSkirmisher = EasterlingSkirmisher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

easterlingSkirmisher :: EnemyCard EasterlingSkirmisher
easterlingSkirmisher = enemy EasterlingSkirmisher Cards.easterlingSkirmisher

-- | Surge. Archery 1.
-- Forced: After Easterling Skirmisher attacks and destroys a character, that character's
-- controller must return the location he controls with the highest threat to the staging
-- area.
instance HasAbilities EasterlingSkirmisher where
  getAbilities (EasterlingSkirmisher a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

instance RunMessage EasterlingSkirmisher where
  runMessage msg e@(EasterlingSkirmisher attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (CharacterDestroyedByAttack _ _ pid) <- ws] (returnHighestThreatLocation attrs)
      pure e
    _ -> EasterlingSkirmisher <$> runMessage msg attrs
