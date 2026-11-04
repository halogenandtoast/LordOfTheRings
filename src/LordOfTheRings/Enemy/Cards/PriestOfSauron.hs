module LordOfTheRings.Enemy.Cards.PriestOfSauron (priestOfSauron) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype PriestOfSauron = PriestOfSauron EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

priestOfSauron :: EnemyCard PriestOfSauron
priestOfSauron = enemy PriestOfSauron Cards.priestOfSauron

-- | Immune to player card effects. Cannot be optionally engaged.
instance HasModifiersFor PriestOfSauron where
  getModifiersFor (PriestOfSauron a) = modifySelf a [ImmuneToPlayerCardEffects, CannotBeOptionallyEngagedBy AnyPlayer]

-- | Forced: After Priest of Sauron attacks and destroys an ally, raise each player's threat
-- by 2.
instance HasAbilities PriestOfSauron where
  getAbilities (PriestOfSauron a) = [mkAbility a 1 $ forced $ AllyDestroyedByAttackWindow After a.id]

instance RunMessage PriestOfSauron where
  runMessage msg e@(PriestOfSauron attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer (`encounterRaisesThreat` 2)
      pure e
    _ -> PriestOfSauron <$> runMessage msg attrs
