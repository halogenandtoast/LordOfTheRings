module LordOfTheRings.Enemy.Cards.ProwlingWolf (prowlingWolf) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype ProwlingWolf = ProwlingWolf EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

prowlingWolf :: EnemyCard ProwlingWolf
prowlingWolf = enemy ProwlingWolf Cards.prowlingWolf

-- | While engaged with a player, Prowling Wolf gets +1 attack for each location in the
-- staging area.
instance HasModifiersFor ProwlingWolf where
  getModifiersFor (ProwlingWolf a) = case a.placement of
    EngagedWith _ -> do
      n <- length <$> select LocationInStagingArea
      when (n > 0) $ modified a a [AttackModifier n]
    _ -> pure ()

-- | Shadow: Defending player exhausts a character he controls.
instance RunMessage ProwlingWolf where
  runMessage msg e@(ProwlingWolf attrs) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure e
    _ -> ProwlingWolf <$> runMessage msg attrs
