module LordOfTheRings.Enemy.Cards.RearguardAmbusher (rearguardAmbusher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype RearguardAmbusher = RearguardAmbusher EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

rearguardAmbusher :: EnemyCard RearguardAmbusher
rearguardAmbusher = enemy RearguardAmbusher Cards.rearguardAmbusher

-- | Cannot be optionally engaged.
instance HasModifiersFor RearguardAmbusher where
  getModifiersFor (RearguardAmbusher a) = modified a a [CannotBeOptionallyEngagedBy AnyPlayer]

-- | Forced: After Rearguard Ambusher engages you, exhaust each damaged character you
-- control.
instance HasAbilities RearguardAmbusher where
  getAbilities (RearguardAmbusher a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage RearguardAmbusher where
  runMessage msg e@(RearguardAmbusher attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        traverse_ exhaust =<< damagedCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
      pure e
    _ -> RearguardAmbusher <$> runMessage msg attrs
