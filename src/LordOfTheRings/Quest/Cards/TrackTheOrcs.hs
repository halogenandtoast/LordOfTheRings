module LordOfTheRings.Quest.Cards.TrackTheOrcs (trackTheOrcs) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWeatherHills.Helpers (revealFromOrcDeck)

newtype TrackTheOrcs = TrackTheOrcs QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
trackTheOrcs :: QuestCard TrackTheOrcs
trackTheOrcs = quest TrackTheOrcs Cards.trackTheOrcs

-- | Each enemy gets +10 engagement cost and cannot be optionally engaged.
instance HasModifiersFor TrackTheOrcs where
  getModifiersFor (TrackTheOrcs a) = do
    enemies <- select AnyEnemy
    for_ enemies \e -> modified a e [EngagementCostModifier 10, CannotBeOptionallyEngagedBy AnyPlayer]

-- | When Revealed: Reveal the top card of the Orc deck (reveal the top 2 cards instead if
-- there are 3 or more players in the game).
instance RunMessage TrackTheOrcs where
  runMessage msg q@(TrackTheOrcs attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getRemainingPlayers
      revealFromOrcDeck (if players >= 3 then 2 else 1)
      pure q
    _ -> TrackTheOrcs <$> runMessage msg attrs
