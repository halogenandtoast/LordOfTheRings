import type { DeckChoice } from './api'
import type { DeckSummary } from './browser/types/deck'

// the starter decks' heroes, as the server deals them, and the sphere each is coloured by:
// the core set's four sphere decks, then the four starter decks
const starters: { name: string; sphere: string; heroCodes: string[]; detail: string }[] = [
  { name: 'leadership', sphere: 'leadership', heroCodes: ['01001', '01002', '01003'], detail: 'newGame.starterDeck' },
  { name: 'tactics', sphere: 'tactics', heroCodes: ['01004', '01005', '01006'], detail: 'newGame.starterDeck' },
  { name: 'spirit', sphere: 'spirit', heroCodes: ['01007', '01008', '01009'], detail: 'newGame.starterDeck' },
  { name: 'lore', sphere: 'lore', heroCodes: ['01010', '01011', '01012'], detail: 'newGame.starterDeck' },
  { name: 'dwarves-of-durin', sphere: 'leadership', heroCodes: ['02116', '131004', '03002'], detail: 'newGame.starterBox' },
  { name: 'elves-of-lorien', sphere: 'lore', heroCodes: ['08001', '08112', '08056'], detail: 'newGame.starterBox' },
  { name: 'defenders-of-gondor', sphere: 'leadership', heroCodes: ['05002', '02050', '08084'], detail: 'newGame.starterBox' },
  { name: 'riders-of-rohan', sphere: 'tactics', heroCodes: ['07001', '17055', '22027'], detail: 'newGame.starterBox' },
]

/** A deck as a string, for v-model: `starter:<sphere>` or `deck:<id>`. */
export type DeckKey = string

export const toDeckChoice = (key: DeckKey): DeckChoice => {
  const [kind, rest] = key.split(':')
  return kind === 'deck' ? { deckId: Number(rest) } : { starter: rest }
}

export interface DeckFace {
  key: DeckKey
  /** A starter's sphere, to colour it; null for one of your decks. */
  sphere: string | null
  /** A starter's name; null for one of your decks. */
  starter: string | null
  name: string | null
  heroCodes: string[]
  detail: string | null
}

export const deckFaces = (decks: DeckSummary[]): DeckFace[] => [
  ...starters.map((s) => ({ key: `starter:${s.name}`, sphere: s.sphere, starter: s.name, name: null, heroCodes: s.heroCodes, detail: s.detail })),
  ...decks.map((d) => ({ key: `deck:${d.id}`, sphere: null, starter: null, name: d.name, heroCodes: d.heroes.map((h) => h.code), detail: d.heroes.map((h) => h.name).join(', ') })),
]
