module LordOfTheRings.Enemy.Cards.FangOfSahir (fangOfSahir) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype FangOfSahir = FangOfSahir EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fangOfSahir :: EnemyCard FangOfSahir
fangOfSahir = enemy FangOfSahir Cards.fangOfSahir

-- | Forced: When Fang of Sahír engages you, either deal 3 damage to a Ship objective you
-- control, or Fang of Sahír makes an immediate attack.
instance HasAbilities FangOfSahir where
  getAbilities (FangOfSahir a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage FangOfSahir where
  runMessage msg e@(FangOfSahir attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        ships <- shipObjectivesOf pid
        chooseOneM pid do
          questionLabeled "fangOfSahir.choose"
          causedBy attrs
          targets ships \s -> dealDamage attrs s 3
          labeled "fangOfSahir.attack" $ immediateAttack pid attrs.id
      pure e
    _ -> FangOfSahir <$> runMessage msg attrs
