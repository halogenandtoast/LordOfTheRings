module LordOfTheRings.Helpers.Drawing (canDrawCardsFrom) where

import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher (getPlayer)
import LordOfTheRings.Modifier (ModifierType (CannotDrawCards, CannotDrawCardsFromCardEffects, CannotDrawCardsFromPlayerCardEffects))
import LordOfTheRings.Prelude
import LordOfTheRings.Source (Source, isPlayerCardSource)
import LordOfTheRings.Target (Target (PlayerTarget))

canDrawCardsFrom :: HasGame m => Source -> PlayerId -> m Bool
canDrawCardsFrom source pid = do
  player <- getPlayer pid
  mods <- getModifiers $ PlayerTarget pid
  pure $ not player.eliminated && notNull player.deck
    && CannotDrawCards `notElem` mods && CannotDrawCardsFromCardEffects `notElem` mods
    && (not (isPlayerCardSource source) || CannotDrawCardsFromPlayerCardEffects `notElem` mods)
