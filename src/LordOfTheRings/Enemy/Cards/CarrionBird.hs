module LordOfTheRings.Enemy.Cards.CarrionBird (carrionBird) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype CarrionBird = CarrionBird EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

carrionBird :: EnemyCard CarrionBird
carrionBird = enemy CarrionBird Cards.carrionBird

-- | Carrion Bird gets -1 engagement cost for each damaged character in play.
instance HasModifiersFor CarrionBird where
  getModifiersFor (CarrionBird a) = do
    n <- length <$> damagedCharacters AnyHero AnyAlly
    when (n > 0) $ modifySelf a [EngagementCostModifier (negate n)]

-- | Forced: After Carrion Bird engages you, discard a damaged character you control.
instance HasAbilities CarrionBird where
  getAbilities (CarrionBird a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage CarrionBird where
  runMessage msg e@(CarrionBird attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        damaged <- filterM (fmap (> 0) . getDamage) =<< charactersControlledBy pid
        chooseOrRunOneM pid do
          questionLabeled "carrionBird.discard"
          causedBy attrs
          targets damaged discard
      pure e
    _ -> CarrionBird <$> runMessage msg attrs
