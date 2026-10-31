module LordOfTheRings.Quest.Cards.BlockedByFlame (blockedByFlame) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightFromMoria.Helpers

newtype BlockedByFlame = BlockedByFlame QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Search for an Exit (Blocked by Flame), from Flight from Moria's nightmare set.
blockedByFlame :: QuestCard BlockedByFlame
blockedByFlame = quest BlockedByFlame Cards.blockedByFlame

-- | The Nameless Fear attacks each player in turn order during the combat phase (deal and
-- discard a new shadow card for each attack). Characters are eligible to attack The
-- Nameless Fear while it is in the staging area: it counts as engaged with each player.
instance HasModifiersFor BlockedByFlame where
  getModifiersFor (BlockedByFlame a) = when a.flipped do
    traverse_ (\nf -> modified a nf [EngagedWithEachPlayer AnyPlayer]) =<< namelessFear

instance HasAbilities BlockedByFlame where
  getAbilities (BlockedByFlame a) = searchForAnExit a <> bypass a

-- | If The Nameless Fear has 10 or more damage on it, remove all damage from it and
-- advance to the next stage. It has no quest points: progress does not defeat it.
instance RunMessage BlockedByFlame where
  runMessage msg q@(BlockedByFlame attrs) = case msg of
    PlaceTokens _ (isTarget attrs -> True) Progress _ -> pure q
    CheckWindows ws | attrs.flipped, any enemyDamaged ws -> do
      namelessFear >>= traverse_ \nf -> do
        damage <- (.damage) <$> getEnemy nf
        when (damage >= 10) do
          removeTokens attrs nf Damage damage
          completeQuest attrs.id
      BlockedByFlame <$> runSearchForAnExit msg attrs
    _ -> BlockedByFlame <$> runSearchForAnExit msg attrs
   where
    enemyDamaged = \case
      Window After (DealtDamage (EnemyTarget _) _) -> True
      _ -> False
