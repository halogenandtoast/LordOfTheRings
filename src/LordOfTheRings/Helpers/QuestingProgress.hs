module LordOfTheRings.Helpers.QuestingProgress (recordQuestingProgress) where

import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Effect
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Message
import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

-- Record actual placement, after a card's prevention and replacement effects.
-- Keeping this on the game also remembers progress on a location just explored.
recordQuestingProgress :: (HasGame m, HasQueue Message m) => Source -> Int -> m ()
recordQuestingProgress source n = when (source == GameSource && n > 0) do
  mods <- getModifiers GameTarget
  when (PlacingQuestingProgress `elem` mods && QuestingProgressWasPlaced `notElem` mods) $
    push $ CreateEffect $ Effect GameSource GameTarget [QuestingProgressWasPlaced] UntilEndOfPhase
