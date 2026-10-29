module LordOfTheRings.Quest.Cards.LostInTheSwanfleetATreacherousSwamp (lostInTheSwanfleetATreacherousSwamp) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheNinInEilph.Helpers (addMarshDweller, runWanderingStage, wanderingStage)

newtype LostInTheSwanfleetATreacherousSwamp = LostInTheSwanfleetATreacherousSwamp QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostInTheSwanfleetATreacherousSwamp :: QuestCard LostInTheSwanfleetATreacherousSwamp
lostInTheSwanfleetATreacherousSwamp = quest LostInTheSwanfleetATreacherousSwamp Cards.lostInTheSwanfleetATreacherousSwamp

instance HasAbilities LostInTheSwanfleetATreacherousSwamp where
  getAbilities (LostInTheSwanfleetATreacherousSwamp a) = wanderingStage a

-- | Each player cannot ready more than 5 characters during the refresh phase.
instance HasModifiersFor LostInTheSwanfleetATreacherousSwamp where
  getModifiersFor (LostInTheSwanfleetATreacherousSwamp a) = when a.flipped do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [ReadyLimitDuringRefresh 5]

-- | When Revealed: Raise each player's threat by 1. Add Ancient Marsh-dweller to the
-- staging area (from out of play or engaged with a player) and heal all damage from it.
instance RunMessage LostInTheSwanfleetATreacherousSwamp where
  runMessage msg q@(LostInTheSwanfleetATreacherousSwamp attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      addMarshDweller
      pure q
    _ -> LostInTheSwanfleetATreacherousSwamp <$> runWanderingStage msg attrs
