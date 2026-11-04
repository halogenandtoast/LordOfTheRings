module LordOfTheRings.Quest.Cards.RallyTheWest (rallyTheWest) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype RallyTheWest = RallyTheWest QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Player side quest. Limit 1 copy of Rally the West in the victory display, on its card def.
rallyTheWest :: QuestCard RallyTheWest
rallyTheWest = quest RallyTheWest Cards.rallyTheWest

-- | While this side quest is in the victory display, each hero gets +1 willpower.
instance HasModifiersFor RallyTheWest where
  getModifiersFor (RallyTheWest a) = when (questInVictoryDisplay a) do
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [WillpowerModifier 1]
