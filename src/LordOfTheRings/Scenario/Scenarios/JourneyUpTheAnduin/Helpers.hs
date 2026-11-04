-- | What the cards of Journey Up the Anduin share.
module LordOfTheRings.Scenario.Scenarios.JourneyUpTheAnduin.Helpers (evilCreatureInPlay) where

import LordOfTheRings.Card (CardDef (cdEncounterSet), toCardDef)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.EncounterSet (EncounterSet (Wilderlands))
import LordOfTheRings.Entities (entitiesEnemies)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Prelude

-- | An enemy from the Evil Creatures deck is in play: Wilderlands' enemies are in no other
-- deck in this scenario (MEC65 rules).
evilCreatureInPlay :: HasGame m => m Bool
evilCreatureInPlay = getsGame (any ((== Just Wilderlands) . cdEncounterSet . toCardDef) . toList . entitiesEnemies . gameEntities)
