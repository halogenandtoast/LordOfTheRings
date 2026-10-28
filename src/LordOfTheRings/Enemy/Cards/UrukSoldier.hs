module LordOfTheRings.Enemy.Cards.UrukSoldier (urukSoldier) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers

newtype UrukSoldier = UrukSoldier EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukSoldier :: EnemyCard UrukSoldier
urukSoldier = enemy UrukSoldier Cards.urukSoldier

-- | Forced: After Uruk Soldier attacks and destroys a character, that character's
-- controller returns a location he controls to the staging area, if able.
instance HasAbilities UrukSoldier where
  getAbilities (UrukSoldier a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: Defending player raises his threat by 1 for each location he controls.
instance RunMessage UrukSoldier where
  runMessage msg e@(UrukSoldier attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (CharacterDestroyedByAttack _ _ pid) <- ws] (returnALocation attrs)
      pure e
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      encounterRaisesThreat pid . length =<< controlledLocations pid
      pure e
    _ -> UrukSoldier <$> runMessage msg attrs
