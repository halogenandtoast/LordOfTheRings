module LordOfTheRings.Objective.Cards.SeizeThem (seizeThem) where

import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Scenario.Scenarios.EscapeFromUmbar.Helpers (mainQuestInPlayAllRound)

newtype SeizeThem = SeizeThem ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

seizeThem :: ObjectiveCard SeizeThem
seizeThem = objective SeizeThem Cards.seizeThem

{- | Forced: After an enemy attacks and destroys a character, remove X progress from the
main quest. X is the attacking enemy's threat. (Once for each attacking enemy, however many
characters its attack destroys.)
At the end of the round, if the main quest has been in play the entire round and there are
no progress tokens on it, the players lose the game.
-}
instance HasAbilities SeizeThem where
  getAbilities (SeizeThem a) =
    [ mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After Nothing
    , mkAbility a 2 $ forced RoundEndsWindow
    ]

instance RunMessage SeizeThem where
  runMessage msg o@(SeizeThem attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (ordNub [eid | Window _ (CharacterDestroyedByAttack eid _ _) <- ws]) \eid -> do
        x <- getEnemyThreat eid
        when (x > 0) $ removeProgressFromMainQuest attrs x
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      allRound <- mainQuestInPlayAllRound
      progress <- getMainQuestProgress
      when (allRound && progress == 0) $ gameOver Lost
      pure o
    _ -> SeizeThem <$> runMessage msg attrs
