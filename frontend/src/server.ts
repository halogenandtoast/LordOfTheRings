// The shapes the server sends. The game is sent whole; ./view.ts turns it into the views the board draws.

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
  | { tag: 'Log'; message: string }
  | { tag: 'Announce'; message: string }
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
}
