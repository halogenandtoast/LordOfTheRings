module LordOfTheRings.Helpers.Doomed (playerCardDoomed) where

import Data.UUID qualified as UUID
import {-# SOURCE #-} LordOfTheRings.Ally (createAlly)
import LordOfTheRings.Ally.Types (AllyAttrs (allyPlacement))
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities (alliesL)
import LordOfTheRings.Game.Base
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Id
import LordOfTheRings.Keyword (Keyword (Doomed))
import LordOfTheRings.Modifier
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Target

-- | Passive text applies as an ally enters, before its printed Doomed triggers:
-- Saruman entering under the Lord of the Dead is blank (FAQ 1.9, p. 17).
-- https://images-cdn.fantasyflightgames.com/filer_public/2e/31/2e3129b3-dc51-4c27-81ed-6a72f13e82f3/lotr_faq_19.pdf
-- Project the actual ally without changing the game or consuming an entity id.
playerCardDoomed :: HasGame m => PlayerId -> Card -> m Int
playerCardDoomed pid card = do
  mods <- case card of
    PlayerCard pc | card.cardType == AllyType -> do
      g <- getGame
      let actor = overAttrs (\a -> a {allyPlacement = InPlayArea pid}) $ createAlly pc (AllyId UUID.nil) pid
          projected = g & entitiesL . alliesL %~ insertEntity actor
          refreshed = runReadGame (preloadModifiers projected) projected
      pure $ runReadGame (getModifiers actor) refreshed
    _ -> getModifiers (CardIdTarget card.id)
  let keywords = [k | Blank `notElem` mods, k <- toList $ cdKeywords $ toCardDef card] <> [k | GainKeyword k <- mods]
  pure $ sum [n | Doomed n <- keywords, LoseKeyword (Doomed n) `notElem` mods]
