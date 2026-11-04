import { errataImage } from './errata'
import type { LogRef } from './gameLog'
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
  /** While it may be declared as an attacker, the attack it would add, bonuses against the enemy included. */
  attackAdding?: number
  /** In hand, an effect forbids playing it: what is forbidding it, which may be unnamed. */
  prohibited?: LogRef[]
  /** Shown as the FAQ's errata reads: its owner plays with errata, or it is an encounter card. */
  errata?: boolean
  /** The code of its facedown side, which this player has looked at (an Uncharted location). */
  otherSide?: string
  /** The image to show instead of its own, as the Stormcaller's stages show their D side. */
  image?: string
}

export interface ChoiceView {
  index: number
  label: string
  targetId: string | null
  /** What choosing it does, for a card's menu when it offers several choices. */
  action: string
  /** Description of this particular ability, shown on its menu option. */
  tooltip?: string
  /** The damage choosing it deals its own card, to preview where that leaves it. */
  damage?: number
  /** The card of a button choice whose card is out of sight, to preview it. */
  card?: CardView
}

/** A search's cards from one zone: a player's deck, hand or discard pile (by name), the
 * encounter deck or discard, the Corsair deck or discard, the victory display, or other. */
export interface SearchGroup {
  zone: string
  player: string | null
  cards: CardView[]
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
  /** The cards a search looked through, by the zone they lie in, when the decision is choosing from them. */
  searched: SearchGroup[] | null
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
  /** The Island Map's locations in their places, the active one and those in the staging area around it marked (Temple of the Deceived). */
  islandMap: IslandMapCell[]
  /** what is under the quest card, as Captured! holds Faramir */
  questAttachments: CardView[]
  staging: CardView[]
  /** While the players are separated, the other areas: each one's players, quest, active location and staging area. */
  otherAreas: AreaView[]
  encounterDeck: number
  encounterDiscard: number
  discard: CardView[]
  victory: CardView[]
  setAside: CardView[]
  victoryPoints: number
  /** The Dream-chaser's heading, lower-cased, once the fleet is prepared. */
  heading: string | null
  /** NormalMode, EasyMode or NightmareMode. */
  mode: string
  /** The Nightmare Deck's setup card, flipped beside the quest deck, in nightmare mode. */
  nightmareCard: CardView | null
  /** The Corsair deck's size, while there is one, and its discard pile. */
  corsairDeck: number | null
  corsairDiscard: CardView[]
  /** The Caves deck's size, in a scenario that has one (Ered Mithrin). */
  cavesDeck: number
  /** The Stormcaller's quest stage, with its progress, and the cards in its area (Flight of the Stormcaller). */
  /** In campaign mode, the campaign card next to the quest deck. */
  campaignCard: CardView | null
  stormcallerStage: CardView | null
  stormcallerArea: CardView[]
  /** The heat tracker's temperature (Desert Crossing). */
  temperature?: number | null
  /** The Orcs' quest stage, with its progress, and the cards in their area (Race Across Harad). */
  orcsStage: CardView | null
  orcsArea: CardView[]
}

export interface IslandMapCell {
  card: CardView
  attachments: CardView[]
  column: number
  row: number
  active: boolean
  staged: boolean
}

export interface AreaView {
  players: string[]
  quest: CardView | null
  activeLocation: CardView | null
  staging: CardView[]
  threat: number
}

export interface PlayerView {
  playerId: string
  name: string
  threat: number
  firstPlayer: boolean
  eliminated: boolean
  handSize: number
  deckSize: number
  /** cards in the player's captured deck (Escape from Mount Gram) */
  capturedDeckSize: number
  discardSize: number
  hand: CardView[]
  discard: CardView[]
  /** treacheries attached to the top of the deck, as Iron Shackles */
  onDeck: CardView[]
  inPlay: CardView[]
  engaged: CardView[]
}

export interface ScoreView {
  threat: number
  deadHeroes: number
  damage: number
  rounds: number
  victoryPoints: number
  total: number
}

export interface GameView {
  score?: ScoreView

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

// a card the engine plays as another kind of card under its own code, showing its printed
// face: Corsair Skirmisher taken as a Ship objective
// and a quest stage printed as sides C and D, as Lost in Mirkwood's 2C, 3C and 4C
const printedFace: Record<string, string> = { '12125-ship': '12125', '18073': '18073-c', '18075': '18075-c', '18077': '18077-c' }
// a quest stage whose B side is printed under the next number, as The Crossroads Fort's 1B,
// or whose other side is D
const printedBack: Record<string, string> = { '17105': '17106-b', '18073': '18073-d', '18075': '18075-d', '18077': '18077-d' }

/** Cards whose side A is another card's: every Search for an Exit shares 03070's. */
const sharedSideA: Record<string, string> = Object.fromEntries(
  [
    '03071', '03072', '03073', '03074', '03075', '03076',
    'khazad-dum-nightmare-flight-from-moria-002',
    'khazad-dum-nightmare-flight-from-moria-003',
    'khazad-dum-nightmare-flight-from-moria-004',
  ].map((code) => [code, '03070']),
)
// a nightmare quest stage printed only as its B side, its A side the standard stage's
const sharedFront: Record<string, string> = {
  'the-nin-in-eilph-nightmare-the-nin-in-eilph-008': '08096',
  'the-nin-in-eilph-nightmare-the-nin-in-eilph-009': '08098',
}

export const cardImage = (card: Pick<CardView, 'code' | 'flipped' | 'facedown' | 'errata' | 'image'>) =>
  card.facedown
    ? `/images/${card.facedown}.jpg`
    : card.image
      ? `/images/cards/${card.image}.avif`
      : !card.flipped && sharedSideA[card.code]
        ? `/images/cards/${sharedSideA[card.code]}.avif`
        : `/images/cards/${errataImage(card.flipped ? (printedBack[card.code] ?? `${printedFace[card.code] ?? card.code}-b`) : (sharedFront[card.code] ?? printedFace[card.code] ?? card.code), card.errata ?? true)}.avif`

/** The drag data type carrying a hand card's id. */
export const cardDrag = 'application/x-lotr-card'
