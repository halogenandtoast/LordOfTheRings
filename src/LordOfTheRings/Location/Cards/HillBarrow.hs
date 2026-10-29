module LordOfTheRings.Location.Cards.HillBarrow (hillBarrow) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Guardian))

newtype HillBarrow = HillBarrow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hillBarrow :: LocationCard HillBarrow
hillBarrow = location HillBarrow Cards.hillBarrow

-- | Forced: When a Guardian enemy attacks, deal it 1 additional shadow card.
instance HasAbilities HillBarrow where
  getAbilities (HillBarrow a) = [mkAbility a 1 $ forced $ MatchingEnemyAttacksWindow When (EnemyWithTrait Guardian)]

instance RunMessage HillBarrow where
  runMessage msg l@(HillBarrow a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ (take 1 [eid | Window _ (EnemyAttacking eid _) <- ws]) dealShadowCard
      pure l
    _ -> HillBarrow <$> runMessage msg a
