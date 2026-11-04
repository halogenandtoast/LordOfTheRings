module LordOfTheRings.Ally.Cards.EastRoadRanger (eastRoadRanger) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)

newtype EastRoadRanger = EastRoadRanger AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

eastRoadRanger :: AllyCard EastRoadRanger
eastRoadRanger = ally EastRoadRanger Cards.eastRoadRanger

-- | East Road Ranger gets +2 willpower while committed to a side quest.
--
-- The side quest chosen as the current quest is read off the lasting effect that makes it
-- so, not the modifiers being gathered alongside this one.
instance HasModifiersFor EastRoadRanger where
  getModifiersFor (EastRoadRanger a) = do
    sideQuests <- getSideQuests
    side <- getsGame (any (\e -> CurrentQuest `elem` effectModifiers e && effectTarget e `elem` sideQuests) . gameEffects)
    when (a.allyCommitted && side) $ modifySelf a [WillpowerModifier 2]
