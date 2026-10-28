module LordOfTheRings.Quest.Cards.AnUntimelyEnd (anUntimelyEnd) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AnUntimelyEnd = AnUntimelyEnd QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

anUntimelyEnd :: QuestCard AnUntimelyEnd
anUntimelyEnd = quest AnUntimelyEnd Cards.anUntimelyEnd

-- | Archery damage must be assigned to allies, if able.
instance HasModifiersFor AnUntimelyEnd where
  getModifiersFor (AnUntimelyEnd a) = when a.flipped $ modified a GameTarget [ArcheryDamageToAllies]
