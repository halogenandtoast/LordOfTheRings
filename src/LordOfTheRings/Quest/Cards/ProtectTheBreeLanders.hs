module LordOfTheRings.Quest.Cards.ProtectTheBreeLanders (protectTheBreeLanders) where

import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ProtectTheBreeLanders = ProtectTheBreeLanders QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge.
protectTheBreeLanders :: QuestCard ProtectTheBreeLanders
protectTheBreeLanders = quest ProtectTheBreeLanders Cards.protectTheBreeLanders

-- | Forced: At the end of the quest phase, if no progress was placed on Protect the
-- Bree-landers, place 1 resource on it. Then, raise each player's threat by the number of
-- resources on Protect the Bree-landers.
instance HasAbilities ProtectTheBreeLanders where
  getAbilities (ProtectTheBreeLanders a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase]

instance RunMessage ProtectTheBreeLanders where
  runMessage msg q@(ProtectTheBreeLanders attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placed <- progressPlacedThisRound (toTarget attrs)
      when (placed == 0) $ placeResources attrs attrs 1
      let n = countTokens Resource attrs.questTokens + fromEnum (placed == 0)
      eachPlayer \p -> encounterRaisesThreat p n
      pure q
    _ -> ProtectTheBreeLanders <$> runMessage msg attrs
