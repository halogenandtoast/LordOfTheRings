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
  /** Time counters (Time X): resource tokens that are not resources. */
  time?: number | null
  damage: number
  exhausted: boolean
  committed: boolean
  /** Considered out of play though on the table, as the card Saruman chooses. */
  outOfPlay?: boolean
  progress: number | null
  questPoints: number | null
  /** The spheres of its resource icons: its own, then any it has gained. */
  spheres: string[]
  attachedTo: string | null
  /** Still in hand while its cost is paid or its host chosen. */
  playing: boolean
  /** The engine's target for a card in play, so debug mode can change it. */
  target: Target | null
  /** The stats down its side as modified, such as ['attack', 3], while in play. */
  stats: [string, number][]
  /** The stats counting toward the quest: committed willpower, staging threat. */
  counting: string[]
  /** While attackers are declared, the attack they would make on it, bonuses included. */
  attackStrength?: number
}

export interface ChoiceView {
  index: number
  label: string
  targetId: string | null
  /** What choosing it does, for a card's menu when it offers several choices. */
  action: string
  /** The damage choosing it deals its own card, to preview where that leaves it. */
  damage?: number
  /** The card of a button choice whose card is out of sight, to preview it. */
  card?: CardView
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
  /** The amounts to pick from, when the decision is an amount: choice 0 is the least. */
  amount: { min: number; max: number } | null
}

export interface ScenarioView {
  name: string
  quest: CardView | null
  activeLocation: CardView | null
  activeLocationAttachments: CardView[]
  /** A second active location, as Ruined Tower makes, with its attachments. */
  otherActiveLocations: { card: CardView; attachments: CardView[] }[]
  /** what is under the quest card, as Captured! holds Faramir */
  questAttachments: CardView[]
  staging: CardView[]
  encounterDeck: number
  encounterDiscard: number
  discard: CardView[]
  victory: CardView[]
  victoryPoints: number
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
  card.facedown ? `/images/${card.facedown}.jpg` : `/images/cards/${card.code}${card.flipped ? '-b' : ''}.avif`

/** The drag data type carrying a hand card's id. */
export const cardDrag = 'application/x-lotr-card'
