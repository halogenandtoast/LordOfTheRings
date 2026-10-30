module LordOfTheRings.Enemy.Cards.CorsairSeafarer (corsairSeafarer) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype CorsairSeafarer = CorsairSeafarer EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairSeafarer :: EnemyCard CorsairSeafarer
corsairSeafarer = enemy CorsairSeafarer Cards.corsairSeafarer

-- | Corsair Seafarer gets +1 attack for each resource on it.
instance HasModifiersFor CorsairSeafarer where
  getModifiersFor (CorsairSeafarer a) = do
    let n = countTokens Resource a.enemyTokens
    when (n > 0) $ modifySelf a [AttackModifier n]

-- | Forced: After the engaged player commits a hero to a Sailing test or to a quest, move 1
-- resource from that hero's resource pool to Corsair Seafarer.
instance HasAbilities CorsairSeafarer where
  getAbilities (CorsairSeafarer a) =
    [ mkAbility a 1 $ forced $ CommittedToSailingTestWindow After
    , mkAbility a 2 $ forced $ AnyCommittedToQuestWindow After
    ]

instance RunMessage CorsairSeafarer where
  runMessage msg e@(CorsairSeafarer attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) _ ws | EngagedWith pid <- attrs.placement -> do
      let committed = [c | Window _ (CommittedToQuest c) <- ws] <> concat [cs | Window _ (CommittedToSailingTest _ cs) <- ws]
      heroes <- select (HeroControlledBy (PlayerWithId pid) <> HeroWithResources)
      for_ [h | h <- heroes, toTarget h `elem` committed] \h -> do
        removeTokens attrs h Resource 1
        placeResources attrs attrs 1
      pure e
    _ -> CorsairSeafarer <$> runMessage msg attrs
