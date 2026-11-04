// The shapes the server sends. The game is sent whole; ./view.ts turns it into the views the board draws.

import type { LogEntry, LogRef } from './gameLog'

export interface CardDef {
  cardCode: string
  name: string
  cardType: string
  threat: number | null
  threatCost: number | null
  willpower: number | null
  attack: number | null
  defense: number | null
  hitPoints: number | null
  questPoints: number | null
  victoryPoints?: number | null
  sphere?: string | null
  cardTraits?: string[]
}

export type Target = { tag: string; contents?: string }
export type Placement = { tag: string; contents?: string | Target }
export type Tokens = Partial<Record<'Damage' | 'Progress' | 'Resource' | 'Villager' | 'TimeCounter', number>>

export interface Entity {
  id: string
  cardId: string
  cardCode: string
  tokens: Tokens
  placement?: Placement
  owner?: string
  exhausted?: boolean
  committed?: boolean
  flipped?: boolean
  /** A hero taken prisoner, face down until rescued. */
  prisoner?: boolean
  /** The shadow cards dealt to an enemy, and those turned face up so far. */
  shadowCards?: Card[]
  shadowRevealed?: string[]
  /** The encounter card guarding an objective. */
  guard?: string | null
  /** The encounter card holding a hero captive. */
  guardedBy?: string | null
  /** The ids of the poison cards attached facedown to a character (Flies and Spiders). */
  poison?: string[]
  /** A character rotated 180 degrees by poison (Flies and Spiders). */
  unconscious?: boolean
  /** A hero in the staging area, under no player's control. */
  inStagingArea?: boolean
  /** A location's facedown side, as an Uncharted location hides it: sent only to the players who have looked at it. */
  otherSide?: string | null
  /** A location's place on the Island Map (Temple of the Deceived). */
  position?: { column: number; row: number } | null
  /** The cards attached to an attachment out of play, as the hero card Thorongil takes, and those faceup under a location, as a Camp's Warden. */
  underneath?: Card[]
}

export interface Card {
  id: string
  cardCode: string
}

export interface UI {
  tag: 'Label' | 'TargetLabel' | 'AbilityLabel'
  label?: string
  target?: Target
  ability?: { abilitySource: Target; abilityIndex: number; abilityType: { tag: string } }
  messages?: Message[]
}

/** The few messages read from a choice, to preview what it does; the rest pass as they are. */
export type Message =
  | { tag: 'DealDamage'; contents: [unknown, Target, number] }
  | { tag: 'Run'; contents: Message[] }
  | { tag: string; contents?: unknown }

export type Question =
  | { tag: 'ChooseOne' | 'ChooseOneAtATime'; choices: UI[] }
  /** Each choice is the amount its index past the least. */
  | { tag: 'ChooseAmount'; minAmount: number; choices: UI[] }
  | { tag: 'QuestionLabel'; label: string; source: Target | null; targets: Target[]; question: Question }
  | { tag: 'SearchCards'; cards: Card[]; question: Question }
  | { tag: 'PlayerWindow'; question: Question }

export interface ServerPlayer {
  id: string
  name: string
  threat: number
  eliminated: boolean
  hand: Card[]
  /** Sent in place of the hand when it is someone else's, at a game with friends. */
  handSize?: number
  discard: Card[]
  deckSize: number
  /** Cards facedown under the threat dial, as Lost in the Dark places them. */
  underThreatDialSize?: number
  /** Whether the player's cards follow the FAQ's errata. */
  errata?: boolean
}

export interface ServerGame {
  score?: import('./types').ScoreView
  players: Record<string, ServerPlayer>
  playerOrder: string[]
  firstPlayer: string
  phase: string
  round: number
  outcome: 'Won' | 'Lost' | null
  question: Record<string, Question>
  questionVersion: number
  scenario: {
    name: string
    encounterDeckSize: number
    encounterDiscard: Card[]
    victoryDisplay: Card[]
    setAside: Card[]
    /** The ids of the hidden cards facedown in each player's play area (The Blood of Gondor). */
    hiddenCards?: Record<string, string[]>
    /** The Dream-chaser's heading, once the fleet is prepared: Sunny, Cloudy, Rainy or Stormy. */
    heading?: string | null
    /** The Corsair deck's size, once prepared, and its own discard pile (Dream-chaser rules). */
    corsairDeckSize?: number
    corsairDiscard?: Card[]
    /** The Caves deck's size (Ered Mithrin): Deep locations are replaced from it. */
    cavesDeckSize?: number
    /** The Evil Creatures deck's size, while there is one, and its own discard pile (Journey Up the Anduin). */
    evilCreaturesDeckSize?: number
    powerOfMordorDeckSize?: number
    sackDeckSize?: number
    evilCreaturesDiscard?: Card[]
    /** The player cards each encounter card, by its card id, guards: a Guarded (X) attachment, or Hobgoblin's card. */
    guardedPlayerCards?: Record<string, [string, Card][]>
    /** NormalMode, EasyMode or NightmareMode; older games carry only easyMode. */
    mode?: string
    /** The scenario's Nightmare Deck, when it has one: its setup card is shown in nightmare mode. */
    nightmareDeck?: { setupCard: string } | null
  }
  /** Where each card a question searches lies: a zone, and the player whose it is. */
  searchZones?: Record<string, { zone: string; player?: string }>
  /** For each attachment in hand, what it could be attached to. */
  attachmentHosts: Record<string, Target[]>
  /** The cards in a hand an effect forbids playing, with what is forbidding them. */
  playProhibitions?: Record<string, LogRef[]>
  entities: Record<'heroes' | 'allies' | 'enemies' | 'locations' | 'attachments' | 'quests' | 'treacheries' | 'objectives', Record<string, Entity>> & {
    // games saved before contracts have none
    contracts?: Record<string, Entity>
  }
  /** Allies that just left play, kept while their own responses to leaving are asked. */
  leftPlay?: Record<string, Entity>
  /** Cards being paid for, by cost id. */
  activeCosts: Record<string, { player: string; card: Card }>
  committedWillpower: number
  stagingThreat: number
  /** While the players are separated: each area's players and cards in play, in turn order, the one in focus marked. */
  areas?: { players: string[]; focused: boolean; threat: number; cards: string[] }[]
  /** The cards adding to the staging threat. */
  threatening?: string[]
  /** The stats of each card in play as modified, in printed order, by card id. */
  stats?: Record<string, [string, number][]>
  /** The hit points of each character and enemy as modified, by card id. */
  hitPoints?: Record<string, number>
  /** While attackers are declared, the attack they would make on each enemy, by card id. */
  attackStrength?: Record<string, number>
  /** While attackers are asked for, the attack each one choosable would add, bonuses against the enemy included, by card id. */
  attackAdding?: Record<string, number>
  /** How many cards lie facedown under each card, by its card id, as underworld cards under a location. */
  underneath?: Record<string, number>
  /** How many player cards lie facedown under each card, by its card id, as the hands Truly Lost places under locations. */
  underneathPlayer?: Record<string, number>
  /** How many captured cards lie under each card, by its card id (Escape from Mount Gram). */
  captured?: Record<string, number>
  /** How many cards are in each player's captured deck, by player id (Escape from Mount Gram). */
  capturedDecks?: Record<string, number>
  /** The top card of each deck played faceup, by player id, as Gandalf (The Road Darkens) has it. */
  deckTops?: Record<string, Card>
  /** In campaign mode, the campaign card next to the quest deck, turned over once read. */
  campaignCard?: { code: string; flipped: boolean } | null
  /** The Stormcaller's quest stage and the progress on it (Flight of the Stormcaller). */
  stormcallerStage?: Card | null
  stormcallerProgress?: number | null
  /** The heat tracker's temperature (Desert Crossing). */
  temperature?: number | null
  /** The side quest deck's size (Fire in the Night). */
  sideQuestDeck?: number
  /** The damage each set-aside card keeps, by its card id, as Cold-Drake set aside by The Withered Heath's stage 3A. */
  setAsideDamage?: Record<string, number>
  /** The Orcs' quest stage and the progress on it (Race Across Harad). */
  orcsStage?: Card | null
  orcsProgress?: number | null
  /** The circuit's six stages, the stages The Challengers and The Wainriders have each advanced through, and the damage on The Wainriders' stage (Challenge of the Wainriders). */
  circuit?: { stages: Card[]; challengers: number; wainriders: number; damage: number } | null
  lastCommittedWillpower: number | null
  /** What each target is modified by, as [target, modifiers] pairs. */
  modifiers?: [Target, { modifierType: { tag: string; contents?: unknown } }[]][]
}

export interface GameData {
  started: boolean
  game: ServerGame | null
  playerId: string | null
  canUndo: boolean
  table: Table
  /** The log's latest entries, when the client should take them afresh. */
  log: LogEntry[] | null
  /** The campaign the game is a scenario of. */
  campaignId?: string | null
}

/** Who plays: with friends, who sits where (seat n plays the nth player); otherwise the owner plays every seat. */
export interface Table {
  withFriends: boolean
  seats: number
  owner: number
  seated: { position: number; userId: number; username: string }[]
}

export type ServerMessage =
  | { tag: 'Update'; contents: GameData; animate?: boolean }
  | { tag: 'Logged'; entry: LogEntry }
  | { tag: 'FlipCard'; cardId: string }
  | { tag: 'ShuffleIntoDeck'; cardIds: string[] }

export interface GameSummary {
  id: string
  name: string
  playerName: string
  scenario: string
  round: number
  phase: string
  threat: number
  outcome: 'Won' | 'Lost' | null
  victoryPoints: number
  /** The group's score, once won. */
  score?: number | null
  /** Waiting for friends to take the rest of the seats. */
  waiting: boolean
  easyMode: boolean
  /** The campaign the game is a scenario of. */
  campaignId?: string | null
  nightmare: boolean
  /** Who plays, in turn order, with their heroes; none until the game is dealt. */
  seats: { name: string; threat: number; eliminated: boolean; heroes: { code: string; sphere: string | null }[] }[]
}
