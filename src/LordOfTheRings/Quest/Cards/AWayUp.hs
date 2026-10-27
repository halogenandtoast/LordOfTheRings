module LordOfTheRings.Quest.Cards.AWayUp (aWayUp) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AWayUp = AWayUp QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

aWayUp :: QuestCard AWayUp
aWayUp = quest AWayUp Cards.aWayUp

-- | Heroes do not collect resources during the resource phase.
-- If players defeat this stage, they win the game.
instance HasModifiersFor AWayUp where
  getModifiersFor (AWayUp a) = do
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [CannotCollectResources]
