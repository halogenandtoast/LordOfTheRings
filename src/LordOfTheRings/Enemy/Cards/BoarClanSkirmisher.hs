module LordOfTheRings.Enemy.Cards.BoarClanSkirmisher (boarClanSkirmisher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype BoarClanSkirmisher = BoarClanSkirmisher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

boarClanSkirmisher :: EnemyCard BoarClanSkirmisher
boarClanSkirmisher = enemy BoarClanSkirmisher Cards.boarClanSkirmisher

-- | Forced: After the engaged player draws any number of cards, he deals 1 damage to a
-- hero he controls.
instance HasAbilities BoarClanSkirmisher where
  getAbilities (BoarClanSkirmisher a) =
    [mkAbility a 1 $ forced $ DrewCardsWindow After (Just pid) | EngagedWith pid <- [a.placement]]

-- | Shadow: Deal 1 damage to a hero you control (2 damage instead if undefended).
instance RunMessage BoarClanSkirmisher where
  runMessage msg e@(BoarClanSkirmisher attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [pid | EngagedWith pid <- [attrs.placement]] \pid -> dealDamageToOneHero attrs pid 1
      pure e
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      dealDamageToOneHero attrs pid (ifUndefended defenders 2 1)
      pure e
    _ -> BoarClanSkirmisher <$> runMessage msg attrs
