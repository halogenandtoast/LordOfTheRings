module LordOfTheRings.Location.Cards.StoneBarrow (stoneBarrow) where

import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Guardian))

newtype StoneBarrow = StoneBarrow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

stoneBarrow :: LocationCard StoneBarrow
stoneBarrow = location StoneBarrow Cards.stoneBarrow

-- | Forced: After a Guardian enemy attacks, raise each player's threat by X, where X is
-- that enemy's threat.
instance HasAbilities StoneBarrow where
  getAbilities (StoneBarrow a) = [mkAbility a 1 $ forced $ MatchingEnemyAttacksWindow After (EnemyWithTrait Guardian)]

instance RunMessage StoneBarrow where
  runMessage msg l@(StoneBarrow a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ (take 1 [eid | Window _ (EnemyAttacking eid _) <- ws]) \eid ->
        whenM (selectAny $ EnemyWithId eid) do
          x <- getEnemyThreat eid
          eachPlayer \p -> encounterRaisesThreat p x
      pure l
    _ -> StoneBarrow <$> runMessage msg a
