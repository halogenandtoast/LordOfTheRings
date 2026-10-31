module LordOfTheRings.Location.Cards.ChamberOfRecords (chamberOfRecords) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Goblin))

newtype ChamberOfRecords = ChamberOfRecords LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

chamberOfRecords :: LocationCard ChamberOfRecords
chamberOfRecords = location ChamberOfRecords Cards.chamberOfRecords

-- | Immune to player card effects.
-- Each Goblin enemy with at least 1 resource on it is immune to player events.
instance HasModifiersFor ChamberOfRecords where
  getModifiersFor (ChamberOfRecords a) = do
    modified a a [ImmuneToPlayerCardEffects]
    goblins <- select (EnemyWithTrait Goblin <> EnemyWithResources)
    for_ goblins \g -> modified a g [ImmuneToPlayerEvents]

-- | Forced: When a Goblin enemy engages a player, add 2 resources to that enemy.
instance HasAbilities ChamberOfRecords where
  getAbilities (ChamberOfRecords a) = [mkAbility a 1 $ forced $ MatchingEnemyEngagedWindow When (EnemyWithTrait Goblin)]

instance RunMessage ChamberOfRecords where
  runMessage msg l@(ChamberOfRecords a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [eid | Window _ (EnemyEngaged eid _) <- ws] \eid -> placeResources a eid 2
      pure l
    _ -> ChamberOfRecords <$> runMessage msg a
