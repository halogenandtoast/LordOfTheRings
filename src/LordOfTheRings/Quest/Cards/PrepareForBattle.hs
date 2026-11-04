module LordOfTheRings.Quest.Cards.PrepareForBattle (prepareForBattle) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype PrepareForBattle = PrepareForBattle QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Player side quest. Limit 1 copy of Prepare for Battle in the victory display, as its
-- card def says.
prepareForBattle :: QuestCard PrepareForBattle
prepareForBattle = quest PrepareForBattle Cards.prepareForBattle

-- | While this quest is in the victory display, the first player draws an additional card
-- during the resource phase.
instance HasModifiersFor PrepareForBattle where
  getModifiersFor (PrepareForBattle a) = whenM (inVictoryDisplay a.cardId) do
    lead <- getLead
    modified a (PlayerTarget lead) [AdditionalResourcePhaseDraws 1]
