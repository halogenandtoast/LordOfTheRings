module LordOfTheRings.Enemy.Cards.MirkwoodBats (mirkwoodBats) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGuard)

newtype MirkwoodBats = MirkwoodBats EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mirkwoodBats :: EnemyCard MirkwoodBats
mirkwoodBats = enemy MirkwoodBats Cards.mirkwoodBats

-- | Surge. Forced: After Mirkwood Bats engages a player, deal 1 damage to each character
-- controlled by the player guarding Gollum.
instance HasAbilities MirkwoodBats where
  getAbilities (MirkwoodBats a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage MirkwoodBats where
  runMessage msg e@(MirkwoodBats attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      getGuard >>= traverse_ \g -> do
        characters <- charactersControlledBy g
        for_ characters \c -> dealDamage attrs c 1
      pure e
    _ -> MirkwoodBats <$> runMessage msg attrs
