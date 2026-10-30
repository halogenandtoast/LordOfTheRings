module LordOfTheRings.Quest.Cards.BreakThroughTheFleet (breakThroughTheFleet) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven.Helpers (raiderFlagship)

newtype BreakThroughTheFleet = BreakThroughTheFleet QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

breakThroughTheFleet :: QuestCard BreakThroughTheFleet
breakThroughTheFleet = quest BreakThroughTheFleet Cards.breakThroughTheFleet

-- | Raider Flagship gets -50 engagement cost. Break Through the Fleet! has +5 quest points
-- for each Ship enemy in play.
instance HasModifiersFor BreakThroughTheFleet where
  getModifiersFor (BreakThroughTheFleet a) = when a.flipped do
    flagships <- select raiderFlagship
    for_ flagships \f -> modified a f [EngagementCostModifier (-50)]
    ships <- length <$> select shipEnemy
    when (ships > 0) $ modified a a [QuestPointModifier (5 * ships)]

{- | When Revealed: The players make engagement checks against each enemy in the staging
area. Raider Flagship gets -50 engagement cost during these engagement checks.
Sailing. If this stage is defeated, or if Raider Flagship is in the victory display, the
players have broken through the Corsair fleet and win the game! Defeated, the quest deck
is empty, and the game is won.
-}
instance RunMessage BreakThroughTheFleet where
  runMessage msg q@(BreakThroughTheFleet attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      BreakThroughTheFleet <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      makeEngagementChecks
      pure q
    -- destroyed, Raider Flagship goes to the victory display
    CheckWindows ws | attrs.flipped, any flagshipDestroyed ws -> do
      winGame
      pure q
    AddToVictoryDisplay (EnemyTarget eid) | attrs.flipped -> do
      isFlagship <- selectAny (EnemyWithId eid <> raiderFlagship)
      when isFlagship winGame
      BreakThroughTheFleet <$> runMessage msg attrs
    _ -> BreakThroughTheFleet <$> runMessage msg attrs
   where
    flagshipDestroyed = \case
      Window After (EnemyDestroyed card) -> toCardCode card == toCardCode Enemies.raiderFlagship
      _ -> False
