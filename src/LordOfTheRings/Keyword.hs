module LordOfTheRings.Keyword (module LordOfTheRings.Keyword) where

import LordOfTheRings.Prelude

data Keyword
  = -- | as the enemy enters play, each player in turn order makes an engagement check
    -- against it alone
    Ambush
  | -- | as the combat phase begins, while in the staging area or on the current quest,
    -- the players deal this much damage among the characters in play
    Archery Int
  | -- | on a quest: characters quest with their attack instead of their willpower
    Battle
  | Doomed Int
  | -- | an objective ally the first player gains control of as it enters play
    FirstPlayerGainsControl
  | Guarded
  | -- | on an enemy: damage never destroys it, as Durin's Bane
    Indestructible
  | Ranged
  | -- | on an enemy: it heals this much damage as the refresh phase begins
    Regenerate Int
  | Restricted
  | -- | on a player card: it costs this much less while its player's threat is 20 or lower
    Secrecy Int
  | Sentinel
  | -- | on a quest: characters quest with their defense instead of their willpower
    Siege
  | Surge
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
