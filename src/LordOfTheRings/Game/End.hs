module LordOfTheRings.Game.End (module LordOfTheRings.Game.End) where

import Data.Map.Strict qualified as Map (findWithDefault)
import LordOfTheRings.Classes.Entity (Entity (toAttrs, toId))
import LordOfTheRings.Classes.HasGame (
  HasGame (getGame),
  getsGame,
 )
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Effect (Effect (effectModifiers))
import LordOfTheRings.Game.Base (
  Game (gameEffects, gameModifiers),
  effectsL,
 )
import LordOfTheRings.GameT (GameT, putGame)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Message (Message (CheckStage, GameOver))
import LordOfTheRings.Modifier (
  Modifier (modifierType),
  ModifierType (
    CannotBeDefeated,
    GameCannotEnd,
    GameEndsWith,
    PlayersCannotWin
  ),
 )
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Prelude hiding (first, many)
import LordOfTheRings.Target (
  Target (GameTarget),
  Targetable (toTarget),
 )

-- | A game that could not end, as Ghulat's, ends with the outcome that waited once it can.
endWaitingGame :: GameT ()
endWaitingGame = do
  g <- getGame
  let waiting = [o | e <- gameEffects g, GameEndsWith o <- effectModifiers e]
      isWaiting = any (\case GameEndsWith _ -> True; _ -> False) . effectModifiers
  for_ (headMay waiting) \outcome -> do
    cannotEnd <- gameCannotEndWith outcome
    unless cannotEnd do
      putGame $ g & effectsL %~ filter (not . isWaiting)
      push $ GameOver outcome

-- | The game is over, or ending as the campaign card's resolution resolves: what the
-- game does by itself between messages, as discarding down to a hand limit, stops.
gameEnding :: Game -> Bool
gameEnding g = isJust g.outcome || (toAttrs g.scenario).inResolution

-- | The game cannot end now with this outcome: it cannot end, or the players cannot win.
gameCannotEndWith :: Outcome -> GameT Bool
gameCannotEndWith outcome = do
  mods <- getModifiers GameTarget
  pure $ GameCannotEnd `elem` mods || (outcome == Won && PlayersCannotWin `elem` mods)

{- | A quest card that could not be defeated, and now can, is checked again: one with
enough progress is defeated. The game given has the modifiers from before the message.
-}
checkUnblockedQuests :: Game -> GameT ()
checkUnblockedQuests before = do
  let wasBlocked q = CannotBeDefeated `elem` map modifierType (Map.findWithDefault [] (toTarget q) (gameModifiers before))
  quests <- getsGame (filter wasBlocked . toList . (.entities.quests))
  for_ quests \q -> do
    blocked <- elem CannotBeDefeated <$> getModifiers (toTarget q)
    unless blocked $ push $ CheckStage (toId q)

