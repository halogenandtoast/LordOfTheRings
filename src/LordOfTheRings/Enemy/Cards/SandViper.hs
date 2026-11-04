module LordOfTheRings.Enemy.Cards.SandViper (sandViper) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype SandViper = SandViper EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

sandViper :: EnemyCard SandViper
sandViper = enemy SandViper Cards.sandViper

-- | Players cannot declare defenders against Sand Viper.
instance HasModifiersFor SandViper where
  getModifiersFor (SandViper a) = modifySelf a [CannotBeDefended]

-- | Forced: After the players travel to a location, Sand Viper engages the first player:
-- from the staging area, or from another player. Engaged with him already, it stays.
instance HasAbilities SandViper where
  getAbilities (SandViper a) = [mkAbility a 1 $ forced $ AnyTraveledWindow After]

instance RunMessage SandViper where
  runMessage msg e@(SandViper attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      unless (attrs.placement == EngagedWith lead) $ engageEnemy lead attrs.id
      pure e
    _ -> SandViper <$> runMessage msg attrs
