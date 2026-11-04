module LordOfTheRings.Quest.Cards.LostUnderground (lostUnderground) where

import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BeneathTheSands.Helpers (getSearchObjective, turnSearchTo)

newtype LostUnderground = LostUnderground QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
lostUnderground :: QuestCard LostUnderground
lostUnderground = quest LostUnderground Cards.lostUnderground

-- | While Lost Underground is in play, the Search objective cannot flip. (Do not resolve
-- the Track keyword: Off Track reads the bar before it resolves it.)
instance HasModifiersFor LostUnderground where
  getModifiersFor (LostUnderground a) = getSearchObjective >>= traverse_ \o -> modified a o [CannotBeFlipped]

{- | When Revealed: Flip the Search objective to Off Track.
Response: When this stage is defeated, flip the Search objective to On Track and ignore its
Forced effect until the end of the phase. The players gain by it, so it is taken, as Find
Water's is; On Track is blank until the end of the phase.
-}
instance RunMessage LostUnderground where
  runMessage msg q@(LostUnderground attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      turnSearchTo Objectives.offTrack
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      turnSearchTo Objectives.onTrack
      getSearchObjective >>= traverse_ \o -> phaseModifier attrs o Blank
      LostUnderground <$> runMessage msg attrs
    _ -> LostUnderground <$> runMessage msg attrs
