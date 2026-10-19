module LordOfTheRings.Quest.Cards.AnduinPassage (anduinPassage) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AnduinPassage = AnduinPassage QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

anduinPassage :: QuestCard AnduinPassage
anduinPassage = quest AnduinPassage Cards.anduinPassage

-- | Reveal 1 additional card from the encounter deck each quest phase. Do not make
-- engagement checks during the encounter phase. (Each player may still optionally engage
-- 1 enemy each encounter phase.)
instance HasModifiersFor AnduinPassage where
  getModifiersFor (AnduinPassage a) = modified a GameTarget [AdditionalStagingReveals 1, NoEngagementChecks]
