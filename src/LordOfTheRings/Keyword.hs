module LordOfTheRings.Keyword (module LordOfTheRings.Keyword) where

import LordOfTheRings.Prelude

data Keyword
  = -- | on a location: it takes 1 damage at the end of each round while in the staging
    -- area, and is destroyed once it has this much (Raid on the Grey Havens)
    Aflame Int
  | -- | as the enemy enters play, each player in turn order makes an engagement check
    -- against it alone
    Ambush
  | -- | as the combat phase begins, while in the staging area or on the current quest,
    -- the players deal this much damage among the characters in play
    Archery Int
  | -- | on a quest: characters quest with their attack instead of their willpower
    Battle
  | -- | on a Ship enemy: engaging a player from the staging area, it reveals this many cards
    -- of the Corsair deck, put into play engaged with him (Dream-chaser rules)
    Boarding Int
  | -- | on an encounter card: as it is revealed it captures this many cards from the top of
    -- its player's captured deck underneath it (Escape from Mount Gram resolves it)
    Capture Int
  | Doomed Int
  | -- | on a player card: it is shuffled into the encounter deck, not a player's deck, and
    -- resolves as an encounter card when revealed, as Ranger of the North (its cards
    -- resolve it)
    Encounter
  | -- | an objective ally the first player gains control of as it enters play
    FirstPlayerGainsControl
  | Guarded
  | -- | an objective ally the last player gains control of as it enters play
    LastPlayerGainsControl
  | -- | on an enemy: damage never destroys it, as Durin's Bane
    Indestructible
  | -- | as the card is revealed from the encounter deck, the players as a group discard
    -- this many resources from their heroes' pools, as many as they can
    Prowl Int
  | Ranged
  | -- | on an enemy: it heals this much damage as the refresh phase begins
    Regenerate Int
  | Restricted
  | -- | on a location: what Across the Ettenmoors' cards ask for, as "a location with the
    -- safe keyword" reads (its cards resolve it)
    Safe
  | -- | does nothing by itself: the card's Scour effect resolves when a card says to trigger
    -- it (Celebrimbor's Secret)
    Scour
  | -- | on a player card: it costs this much less while its player's threat is 20 or lower
    Secrecy Int
  | -- | on a quest stage: the first player makes a Sailing test at the start of each quest
    -- phase (Dream-chaser rules)
    Sailing
  | Sentinel
  | -- | on a quest: characters quest with their defense instead of their willpower
    Siege
  | Surge
  | -- | on a location: it enters play with this side faceup, its other side facedown, and
    -- is flipped to it (The Grey Havens rules insert)
    Uncharted
  | -- | on a quest, location or enemy: it gets this many time counters as it is revealed,
    -- and loses 1 at the end of each refresh phase
    Time Int
  | -- | on an enemy: engaged with a player, it is dealt no shadow card and makes no attack
    -- in the combat phase; as that phase begins, its player removes 1 progress from the
    -- current quest, or from the active location once the quest has none
    Hinder
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
