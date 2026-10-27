-- | What the cards of The Watcher in the Water share: the tentacles' check.
module LordOfTheRings.Scenario.Scenarios.TheWatcherInTheWater.Helpers (isTentacleEnemy, tentacleStrikes) where

import LordOfTheRings.Card (CardDef (..), EncounterCard, toCardDef)
import LordOfTheRings.Card.CardType (CardType (EnemyType))
import LordOfTheRings.Prelude
import LordOfTheRings.Trait (Trait (Tentacle))

-- | A Tentacle enemy card.
isTentacleEnemy :: EncounterCard -> Bool
isTentacleEnemy (toCardDef -> def) = cdCardType def == EnemyType && Tentacle `member` cdCardTraits def

{- | Whether the cards a tentacle's Forced effect discarded from the encounter deck set it
off: "if that card has a shadow effect or is a Tentacle enemy".
-}
tentacleStrikes :: [EncounterCard] -> Bool
tentacleStrikes = any \c -> cdShadowEffect (toCardDef c) || isTentacleEnemy c
