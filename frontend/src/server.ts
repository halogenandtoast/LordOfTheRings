// The shapes the server sends. The game is sent whole; ./view.ts turns it into the views the board draws.

import type { LogEntry } from './gameLog'

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
  sphere?: string | null
}

export type Target = { tag: string; contents?: string }
export type Placement = { tag: string; contents?: string | Target }
export type Tokens = Partial<Record<'Damage' | 'Progress' | 'Resource', number>>

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
}

export interface Card {
  id: string
  cardCode: string
}

export interface UI {
  tag: 'Label' | 'TargetLabel' | 'AbilityLabel'
  label?: string
  target?: Target
  ability?: { abilitySource: Target; abilityType: { tag: string } }
}

export type Question =
  | { tag: 'ChooseOne' | 'ChooseOneAtATime'; choices: UI[] }
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
}

export interface ServerGame {
  players: Record<string, ServerPlayer>
  playerOrder: string[]
  firstPlayer: string
  phase: string
  round: number
  outcome: 'Won' | 'Lost' | null
  question: Record<string, Question>
  questionVersion: number
  scenario: { name: string; encounterDeckSize: number; encounterDiscard: Card[]; victoryDisplay: Card[] }
  /** For each attachment in hand, what it could be attached to. */
  attachmentHosts: Record<string, Target[]>
  entities: Record<'heroes' | 'allies' | 'enemies' | 'locations' | 'attachments' | 'quests' | 'treacheries' | 'objectives', Record<string, Entity>>
  /** Cards being paid for, by cost id. */
  activeCosts: Record<string, { player: string; card: Card }>
  committedWillpower: number
  stagingThreat: number
  /** The stats of each card in play as modified, in printed order, by card id. */
  stats?: Record<string, [string, number][]>
  /** The hit points of each character and enemy as modified, by card id. */
  hitPoints?: Record<string, number>
  /** While attackers are declared, the attack they would make on each enemy, by card id. */
  attackStrength?: Record<string, number>
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
}

/** Who plays: with friends, who sits where (seat n plays the nth player); otherwise the owner plays every seat. */
export interface Table {
  withFriends: boolean
  seats: number
  owner: number
  seated: { position: number; userId: number; username: string }[]
}

export type ServerMessage =
  | { tag: 'Update'; contents: GameData }
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
  /** Waiting for friends to take the rest of the seats. */
  waiting: boolean
  easyMode: boolean
  /** Who plays, in turn order, with their heroes; none until the game is dealt. */
  seats: { name: string; threat: number; eliminated: boolean; heroes: { code: string; sphere: string | null }[] }[]
}
