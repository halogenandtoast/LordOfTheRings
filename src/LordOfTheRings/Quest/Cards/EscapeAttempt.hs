module LordOfTheRings.Quest.Cards.EscapeAttempt (escapeAttempt) where

import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGuard)

newtype EscapeAttempt = EscapeAttempt QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

escapeAttempt :: QuestCard EscapeAttempt
escapeAttempt = quest EscapeAttempt Cards.escapeAttempt

-- | The player guarding Gollum cannot commit characters to this quest (unless he is the
-- only player remaining in the game).
instance HasModifiersFor EscapeAttempt where
  getModifiersFor (EscapeAttempt a) = do
    players <- length <$> getRemainingPlayers
    guard' <- getGuard
    for_ guard' \g -> when (players > 1) $ modified a (PlayerTarget g) [CannotCommitCharacters]

-- | If the players quest unsuccessfully, Gollum escapes and the players have lost the game.
instance HasAbilities EscapeAttempt where
  getAbilities (EscapeAttempt a) = [mkAbility a 1 $ forced $ QuestedUnsuccessfullyWindow After]

instance RunMessage EscapeAttempt where
  runMessage msg q@(EscapeAttempt attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      gameOver Lost
      pure q
    _ -> EscapeAttempt <$> runMessage msg attrs
