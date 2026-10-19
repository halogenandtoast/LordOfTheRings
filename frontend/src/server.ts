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
  | { tag: 'QuestionLabel'; label: string; question: Question }

export interface ServerPlayer {
  id: string
  name: string
  threat: number
  eliminated: boolean
  hand: Card[]
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
  scenario: { name: string; encounterDeckSize: number; encounterDiscard: Card[] }
  entities: Record<'heroes' | 'allies' | 'enemies' | 'locations' | 'attachments' | 'quests' | 'treacheries', Record<string, Entity>>
}

export interface GameData {
  started: boolean
  game: ServerGame | null
  playerId: string | null
  canUndo: boolean
}

export type ServerMessage =
  | { tag: 'Update'; contents: GameData }
  | { tag: 'Log'; message: string }
  | { tag: 'Announce'; message: string }
  | { tag: 'FlipCard'; cardId: string }
