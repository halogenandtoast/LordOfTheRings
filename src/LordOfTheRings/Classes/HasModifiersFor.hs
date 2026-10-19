module LordOfTheRings.Classes.HasModifiersFor (module LordOfTheRings.Classes.HasModifiersFor) where

import Control.Monad.Writer.Class
import Data.Map.Monoidal.Strict (MonoidalMap (..))
import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

type HasModifiersM m = (HasGame m, MonadWriter (MonoidalMap Target [Modifier]) m)

class HasModifiersFor a where
  getModifiersFor :: (HasModifiersM m) => a -> m ()
  getModifiersFor _ = pure ()

modifySelf :: (HasModifiersM m, Targetable a, Sourceable a) => a -> [ModifierType] -> m ()
modifySelf a = modified a a

modified :: (HasModifiersM m, Targetable target, Sourceable source) => source -> target -> [ModifierType] -> m ()
modified source target mods =
  tell $ MonoidalMap $ Map.singleton (toTarget target) (map (Modifier (toSource source)) mods)
