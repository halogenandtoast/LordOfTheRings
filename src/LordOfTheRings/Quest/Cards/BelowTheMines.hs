{- | The four stage 4s. Each player draws one at Washed Away!, which resolves their When
Revealed effects: see "LordOfTheRings.Scenario.Scenarios.FoundationsOfStone.Helpers".

Forced: After the last progress token is placed on this stage, join another player at
the beginning of the travel phase. If you cannot join another player, all players continue
on to stage 5 together. (The players share one staging area and stage, so they move on to
the next drawn stage 4, and after the last to stage 5.)
-}
module LordOfTheRings.Quest.Cards.BelowTheMines (
  belowTheMines,
  theEndlessCaves,
  theShiveringBank,
  oldOneLair,
) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype BelowTheMines = BelowTheMines QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sheltered Rocks.
belowTheMines :: QuestCard BelowTheMines
belowTheMines = quest BelowTheMines Cards.belowTheMines

theEndlessCaves :: QuestCard BelowTheMines
theEndlessCaves = quest BelowTheMines Cards.theEndlessCaves

theShiveringBank :: QuestCard BelowTheMines
theShiveringBank = quest BelowTheMines Cards.theShiveringBank

oldOneLair :: QuestCard BelowTheMines
oldOneLair = quest BelowTheMines Cards.oldOneLair
