module LordOfTheRings.Enemy.Cards.BoarClanStalker (boarClanStalker) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype BoarClanStalker = BoarClanStalker EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

boarClanStalker :: EnemyCard BoarClanStalker
boarClanStalker = enemy BoarClanStalker Cards.boarClanStalker

-- | Boar Clan Stalker gets +1 attack and +1 defense for each resource token on it (limit
-- +4).
instance HasModifiersFor BoarClanStalker where
  getModifiersFor (BoarClanStalker a) = do
    let n = min 4 $ countTokens Resource a.enemyTokens
    when (n > 0) $ modified a a [AttackModifier n, DefenseModifier n]

-- | Forced: After the engaged player draws any number of cards, put a resource token on
-- Boar Clan Stalker.
instance HasAbilities BoarClanStalker where
  getAbilities (BoarClanStalker a) =
    [mkAbility a 1 $ forced $ DrewCardsWindow After (Just pid) | EngagedWith pid <- [a.placement]]

instance RunMessage BoarClanStalker where
  runMessage msg e@(BoarClanStalker attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      pure e
    _ -> BoarClanStalker <$> runMessage msg attrs
