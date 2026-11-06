module LordOfTheRings.Keyword (module LordOfTheRings.Keyword) where

import LordOfTheRings.Card.CardType (CardType)
import LordOfTheRings.Prelude

data Keyword
  = -- | after an enemy attacks, discard this many cards from the defending player's deck
    Pillage Int
  | -- | stays in staging, is engaged with every player, and attacks each in turn
    Massive
  | -- | on a location: it takes 1 damage at the end of each round while in the staging
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
  | -- | on a location: contributes this much when Lake-town resolves Burn damage
    Burn Int
  | -- | on a Ship enemy: engaging a player from the staging area, it reveals this many cards
    -- of the Corsair deck, put into play engaged with him (Dream-chaser rules)
    Boarding Int
  | -- | on an encounter card: as it is revealed it captures this many cards from the top of
    -- its player's captured deck underneath it (Escape from Mount Gram resolves it)
    Capture Int
  | -- | on a main quest: each player's threat elimination level is 99, their threat cannot
    -- be reduced by more than 1 each round by non-boon player card effects, and Valour
    -- triggers at 80 threat, not 40 (The Mountain of Fire rules insert, "Dire")
    Dire
  | -- | on a quest stage: questing successfully places no progress, and questing
    -- unsuccessfully raises no threat: the first player places the difference as progress
    -- on the current quest instead, the active location taking it first (The Treason of
    -- Saruman rules insert, "Defense")
    DefenseKeyword
  | Doomed Int
  | -- | on a player card: it is shuffled into the encounter deck, not a player's deck, and
    -- resolves as an encounter card when revealed, as Ranger of the North (its cards
    -- resolve it)
    Encounter
  | -- | an objective ally the first player gains control of as it enters play
    FirstPlayerGainsControl
  | -- | on a Tentacle enemy: revealed, it attaches facedown to the location in play its
    -- parenthetical picks, grappled: out of the staging area, unengageable, immune to
    -- player card effects, its location +2 threat, until the location becomes active or
    -- is explored (The Thing in the Depths)
    Grapple GrappleLocation
  | Guarded
  | -- | on an objective: Guarded (X), as The Black Serpent's Harad objectives read. As it is
    -- revealed, cards are discarded from the encounter deck until one of these types is,
    -- which goes to the staging area with the objective attached to it (MEC59 rules)
    GuardedBy [CardType]
  | -- | an objective ally the last player gains control of as it enters play
    LastPlayerGainsControl
  | -- | on an encounter card: the player who reveals it makes a Hide test against this many
    -- encounter cards, before the rest of the card resolves (The Black Riders rules insert,
    -- "Hide X")
    Hide Int
  | -- | on an enemy: damage never destroys it, as Durin's Bane
    Indestructible
  | -- | on an encounter card: the player who reveals it resolves its staging on his own,
    -- the other players taking no actions and triggering no responses meanwhile (The Black
    -- Riders rules insert)
    Peril
  | -- | on an enemy: the damage its attack would deal is canceled, and the defending player
    -- raises his threat by as much instead (The Flame of the West rules insert)
    Phantom
  | -- | on a boon or burden: earned, it is attached to a hero the campaign log records. It
    -- cannot be discarded from that hero while the hero is in play, and is removed from the
    -- game as the hero leaves play (The Black Riders rules insert)
    Permanent
  | -- | on an enemy: each time it is assigned damage, it takes this much less (The Road
    -- Darkens rules insert)
    Toughness Int
  | -- | as the card is revealed from the encounter deck, the players as a group discard
    -- this many resources from their heroes' pools, as many as they can
    Prowl Int
  | -- | on a quest stage: it cannot be defeated outside of the quest phase, player card
    -- effects cannot place progress on it, and defeated it stays in play (MEC80 rules,
    -- "Race Keyword"; Challenge of the Wainriders resolves it)
    Race
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
  | -- | on a location: after the players travel to it while the Search objective is Off
    -- Track, they discard this many encounter cards, flipping it On Track if one has On
    -- Track printed (Beneath the Sands resolves it)
    Track Int
  | -- | on a quest, location or enemy: it gets this many time counters as it is revealed,
    -- and loses 1 at the end of each refresh phase
    Time Int
  | -- | on an enemy: a character it damages is given 1 poison by its controller (The
    -- Hobbit: On the Doorstep rules insert)
    Venom
  | -- | on an enemy: engaged with a player, it is dealt no shadow card and makes no attack
    -- in the combat phase; as that phase begins, its player removes 1 progress from the
    -- current quest, or from the active location once the quest has none
    Hinder
  | -- | on a location: after the players travel to it, it is discarded and the top card of
    -- the Caves deck takes its place as the active location (Ered Mithrin rules)
    Deep
  | -- | on a location: as it becomes the active location, the first player looks at this
    -- many encounter cards, attaches a Loot objective among them to it as a guarded
    -- objective, reveals a random Hazard card among them, and discards the rest (MEC69
    -- rules)
    Discover Int
  | -- | on an objective: one revealed from the encounter deck is discarded and another card
    -- revealed; it enters play only by the Discover keyword (MEC69 rules)
    Loot
  | -- | on a location: at the end of each refresh phase it takes 1 resource, a "mire
    -- token", while in the staging area, and it is discarded as soon as it has this many,
    -- its Forced effect for being discarded by the Mire keyword answering (The Land of
    -- Shadow rules insert; the revised Two Towers rulebook, p. 9, places them on locations
    -- with the keyword alone)
    Mire Int
  | -- | on an encounter card with a player card back, shuffled into a player's deck: drawn,
    -- it stays in his hand, it can be discarded from his hand or deck to his discard pile,
    -- and it cannot be played, as The Searching Eye reads "functions like a player card"
    -- (The Land of Shadow rules insert; the revised Two Towers rulebook, p. 5)
    FunctionsLikeAPlayerCard
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The location a Grapple enemy attaches to, by its printed threat or quest points.
data GrappleLocation
  = LowestPrintedThreat
  | HighestPrintedThreat
  | LowestPrintedQuestPoints
  | HighestPrintedQuestPoints
  | -- | the location with the most enemies grappled with it, or the fewest
    MostEnemiesGrappled
  | FewestEnemiesGrappled
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Whether the keyword is Grapple, whatever its parenthetical.
isGrapple :: Keyword -> Bool
isGrapple = \case
  Grapple _ -> True
  _ -> False
