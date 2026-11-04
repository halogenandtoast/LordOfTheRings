module LordOfTheRings.Location.Cards.RedhornPass (redhornPass) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype RedhornPass = RedhornPass LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

redhornPass :: LocationCard RedhornPass
redhornPass = location RedhornPass Cards.redhornPass

-- | Immune to player card effects.
instance HasModifiersFor RedhornPass where
  getModifiersFor (RedhornPass a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: When Redhorn Pass is explored, each player assigns X damage among characters he
-- controls. X is the number of damage here.
instance HasAbilities RedhornPass where
  getAbilities (RedhornPass a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage RedhornPass where
  runMessage msg l@(RedhornPass a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      when (a.damage > 0) $ eachPlayer \pid -> dealDamageDivided a pid a.damage
      pure l
    _ -> RedhornPass <$> runMessage msg a
