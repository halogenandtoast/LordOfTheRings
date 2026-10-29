module LordOfTheRings.Enemy.Cards.BoarClanWarrior (boarClanWarrior) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype BoarClanWarrior = BoarClanWarrior EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

boarClanWarrior :: EnemyCard BoarClanWarrior
boarClanWarrior = enemy BoarClanWarrior Cards.boarClanWarrior

-- | Forced: After the engaged player draws any number of cards, deal Boar Clan Warrior a
-- shadow card.
instance HasAbilities BoarClanWarrior where
  getAbilities (BoarClanWarrior a) =
    [mkAbility a 1 $ forced $ DrewCardsWindow After (Just pid) | EngagedWith pid <- [a.placement]]

-- | Shadow: Attacking enemy gets +1 attack for each shadow card on it.
instance RunMessage BoarClanWarrior where
  runMessage msg e@(BoarClanWarrior attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      dealShadowCard attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      shadows <- length . (.shadowCards) <$> getEnemy eid
      enemyGetsAttack attrs eid shadows
      pure e
    _ -> BoarClanWarrior <$> runMessage msg attrs
