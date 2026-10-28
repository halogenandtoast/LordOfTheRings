module LordOfTheRings.Quest.Cards.ReinforcingTheBanks (reinforcingTheBanks) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ReinforcingTheBanks = ReinforcingTheBanks QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

reinforcingTheBanks :: QuestCard ReinforcingTheBanks
reinforcingTheBanks = quest ReinforcingTheBanks Cards.reinforcingTheBanks

-- | Reveal 1 additional card from the encounter deck and add it to the staging area during
-- the staging step each round.
instance HasModifiersFor ReinforcingTheBanks where
  getModifiersFor (ReinforcingTheBanks a) = modified a GameTarget [AdditionalStagingReveals 1]
