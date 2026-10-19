import type { Target } from './server'

export interface CardView {
  id: string
  code: string
  name: string
  type: string
  flipped: boolean
  /** Face down: a prisoner, or a player card fighting as an Orc Guard. */
  facedown?: 'player' | 'encounter'
  threat: number | null
  attack: number | null
  defense: number | null
  hitPoints: number | null
  willpower: number | null
  resources: number | null
  damage: number
  exhausted: boolean
  committed: boolean
  progress: number | null
  questPoints: number | null
  attachedTo: string | null
  /** Still in hand while its cost is paid or its host chosen. */
  playing: boolean
  /** The engine's target for a card in play, so debug mode can change it. */
  target: Target | null
}

export interface ChoiceView {
  index: number
  label: string
  targetId: string | null
}

export interface QuestionView {
  askId: number
  playerId: string
  prompt: string
  /** The card causing this decision. */
  sourceId: string | null
  /** The cards the decision is aimed at. */
  targetIds: string[]
  choices: ChoiceView[]
  /** The cards a search looked through, when the decision is choosing from them. */
  searched: CardView[] | null
}

export interface ScenarioView {
  name: string
  quest: CardView | null
  activeLocation: CardView | null
  staging: CardView[]
  encounterDeck: number
  encounterDiscard: number
  discard: CardView[]
}

export interface PlayerView {
  playerId: string
  name: string
  threat: number
  firstPlayer: boolean
  eliminated: boolean
  handSize: number
  deckSize: number
  discardSize: number
  hand: CardView[]
  discard: CardView[]
  inPlay: CardView[]
  engaged: CardView[]
}

export interface GameView {
  started: boolean
  round: number
  phase: string | null
  outcome: 'WON' | 'LOST' | null
  over: boolean
  question: QuestionView | null
  /** The cards each attachment in hand could be attached to. */
  attachmentHosts: Record<string, string[]>
  scenario: ScenarioView | null
  players: PlayerView[]
  tally: TallyView | null
}

export interface TallyView {
  willpower: number
  threat: number
  lastWillpower: number | null
}

export const cardImage = (card: Pick<CardView, 'code' | 'flipped' | 'facedown'>) =>
  card.facedown ? `/images/${card.facedown}.jpg` : `/images/cards/${card.code}${card.flipped ? '-b' : ''}.jpg`

/** The drag data type carrying a hand card's id. */
export const cardDrag = 'application/x-lotr-card'
