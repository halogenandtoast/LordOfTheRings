module LordOfTheRings.Enemy.Cards.TreacherousEasterling (treacherousEasterling) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype TreacherousEasterling = TreacherousEasterling EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

treacherousEasterling :: EnemyCard TreacherousEasterling
treacherousEasterling = enemy TreacherousEasterling Cards.treacherousEasterling

-- | Treacherous Easterling gets +1 Attack and +1 Defense for each resource token on it.
-- (Limit +3 Attack and +3 Defense.)
instance HasModifiersFor TreacherousEasterling where
  getModifiersFor (TreacherousEasterling a) = do
    let n = min 3 $ countTokens Resource a.enemyTokens
    when (n > 0) $ modifySelf a [AttackModifier n, DefenseModifier n]

-- | Forced: After the engaged player raises his threat, place 1 resource token on
-- Treacherous Easterling.
instance HasAbilities TreacherousEasterling where
  getAbilities (TreacherousEasterling a) = case a.placement of
    EngagedWith pid -> [mkAbility a 1 $ forced $ ThreatRaisedWindow After (PlayerWithId pid)]
    _ -> []

instance RunMessage TreacherousEasterling where
  runMessage msg e@(TreacherousEasterling attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      pure e
    _ -> TreacherousEasterling <$> runMessage msg attrs
