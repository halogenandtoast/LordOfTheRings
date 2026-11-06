module LordOfTheRings.Ally (createAlly) where

import LordOfTheRings.Ally.Types (Ally)
import LordOfTheRings.Card.PlayerCard (PlayerCard)
import LordOfTheRings.Id (AllyId, PlayerId)

createAlly :: PlayerCard -> AllyId -> PlayerId -> Ally
