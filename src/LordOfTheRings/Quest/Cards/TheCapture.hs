module LordOfTheRings.Quest.Cards.TheCapture (theCapture) where

import LordOfTheRings.Card (EncounterCard (..), genEncounterCard, toCardCode)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDeadMarshes.Helpers (getGollum)

newtype TheCapture = TheCapture QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCapture :: QuestCard TheCapture
theCapture = quest TheCapture Cards.theCapture

{- | Forced: After this stage is defeated, the first player chooses a player. That player
must pass an Escape test, dealing 1 card from the encounter deck for each resource token
on Gollum, to capture him. If Gollum is not captured at this time, reset the quest deck
to stage 1B. If this final Escape test is passed, the players have captured Gollum and
won the game.
-}
instance RunMessage TheCapture where
  runMessage msg (TheCapture attrs) = case msg of
    PlaceTokens _ (isTarget attrs -> True) Progress n | attrs.progress + n >= attrs.questPoints -> do
      lead <- getLead
      gollum <- getGollum
      resources <- maybe (pure 0) (fmap (.resources) . getObjective) gollum
      stage1 <- genEncounterCard Cards.intoTheMarshes
      choosePlayerM lead attrs \p ->
        escapeTest
          p
          attrs
          resources
          EscapeWillpower
          (resetToStage stage1 [MkEncounterCard attrs.cardId (toCardCode attrs)])
          winGame
      pure $ TheCapture $ attrs & tokensL %~ addTokens Progress n
    _ -> TheCapture <$> runMessage msg attrs
