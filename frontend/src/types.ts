export interface CardView {
  id: string
  code: string
  name: string
  type: string
  flipped: boolean
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
  scenario: ScenarioView | null
  players: PlayerView[]
}

export const cardImage = (card: Pick<CardView, 'code' | 'flipped'>) =>
  `/images/cards/${card.code}${card.flipped ? '-b' : ''}.jpg`
