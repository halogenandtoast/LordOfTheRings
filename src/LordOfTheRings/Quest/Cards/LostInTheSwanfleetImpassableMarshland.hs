module LordOfTheRings.Quest.Cards.LostInTheSwanfleetImpassableMarshland (lostInTheSwanfleetImpassableMarshland) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheNinInEilph.Helpers (addMarshDweller, runWanderingStage, wanderingStage)

newtype LostInTheSwanfleetImpassableMarshland = LostInTheSwanfleetImpassableMarshland QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostInTheSwanfleetImpassableMarshland :: QuestCard LostInTheSwanfleetImpassableMarshland
lostInTheSwanfleetImpassableMarshland = quest LostInTheSwanfleetImpassableMarshland Cards.lostInTheSwanfleetImpassableMarshland

instance HasAbilities LostInTheSwanfleetImpassableMarshland where
  getAbilities (LostInTheSwanfleetImpassableMarshland a) = wanderingStage a

-- | Each location in play gets +1 threat.
instance HasModifiersFor LostInTheSwanfleetImpassableMarshland where
  getModifiersFor (LostInTheSwanfleetImpassableMarshland a) = when a.flipped do
    locations <- select AnyLocation
    for_ locations \l -> modified a l [ThreatModifier 1]

-- | When Revealed: Raise each player's threat by 1. Add Ancient Marsh-dweller to the
-- staging area (from out of play or engaged with a player) and heal all damage from it.
instance RunMessage LostInTheSwanfleetImpassableMarshland where
  runMessage msg q@(LostInTheSwanfleetImpassableMarshland attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      addMarshDweller
      pure q
    _ -> LostInTheSwanfleetImpassableMarshland <$> runWanderingStage msg attrs
