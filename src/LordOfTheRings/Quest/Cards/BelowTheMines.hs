{- | The four stage 4s. Each player draws one at Washed Away!, which resolves their When
Revealed effects: see "LordOfTheRings.Scenario.Scenarios.FoundationsOfStone.Helpers".

Forced: After the last progress token is placed on this stage, join another player at
the beginning of the travel phase. If you cannot join another player, all players continue
on to stage 5 together. (Joining is the engine's, at the beginning of the travel phase;
once the players are together again, the stage advances to stage 5 as any other does.)
-}
module LordOfTheRings.Quest.Cards.BelowTheMines (
  belowTheMines,
  theEndlessCaves,
  theShiveringBank,
  oldOneLair,
) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameSeparation)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype BelowTheMines = BelowTheMines QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
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

instance RunMessage BelowTheMines where
  runMessage msg (BelowTheMines attrs) = case msg of
    PlaceTokens _ (isTarget attrs -> True) Progress n -> do
      separated <- getsGame (isJust . gameSeparation)
      if separated
        then do
          when (attrs.progress < attrs.questPoints && attrs.progress + n >= attrs.questPoints) joinAnotherPlayer
          pure $ BelowTheMines $ attrs & tokensL %~ addTokens Progress n
        else BelowTheMines <$> runMessage msg attrs
    _ -> BelowTheMines <$> runMessage msg attrs
