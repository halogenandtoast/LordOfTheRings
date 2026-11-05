module LordOfTheRings.Location.Cards.CandleLitPool (candleLitPool) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (searchDeckAndDiscardFor, undeadEnemy)
import LordOfTheRings.Sphere (Sphere (Neutral))

newtype CandleLitPool = CandleLitPool LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

candleLitPool :: LocationCard CandleLitPool
candleLitPool = location CandleLitPool Cards.candleLitPool

instance HasModifiersFor CandleLitPool where
  getModifiersFor (CandleLitPool a) = do
    available <- getGroupResourcesOfSphere Neutral
    when (available < 2) $ modifySelf a [CannotBeTraveledTo]

instance HasAbilities CandleLitPool where
  getAbilities (CandleLitPool a) = [mkAbility a 1 $ forced $ DiscardedByMireWindow When a.id]

instance RunMessage CandleLitPool where
  runMessage msg l@(CandleLitPool a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      spendGroupResources lead Neutral 2
      CandleLitPool <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      -- Each search reads the piles after the preceding player's choice resolves.
      eachPlayer \pid -> doStep 1 (UseCardAbility pid (toSource a) 1 ws)
      pure l
    DoStep 1 (UseCardAbility pid (isSource a -> True) 1 _) -> do
      searchDeckAndDiscardFor pid "candleLitPool.search" undeadEnemy (putEnemyIntoPlayEngaged pid)
      pure l
    _ -> CandleLitPool <$> runMessage msg a
