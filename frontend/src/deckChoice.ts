import type { DeckChoice } from './api'
import type { DeckSummary } from './browser/types/deck'

export const starterSpheres = ['leadership', 'tactics', 'spirit', 'lore'] as const

// the core set's starter heroes, as the server deals them
const starterHeroes: Record<string, string[]> = {
  leadership: ['01001', '01002', '01003'],
  tactics: ['01004', '01005', '01006'],
  spirit: ['01007', '01008', '01009'],
  lore: ['01010', '01011', '01012'],
}

/** A deck as a string, for v-model: `starter:<sphere>` or `deck:<id>`. */
export type DeckKey = string

export const toDeckChoice = (key: DeckKey): DeckChoice => {
  const [kind, rest] = key.split(':')
  return kind === 'deck' ? { deckId: Number(rest) } : { starter: rest }
}

export interface DeckFace {
  key: DeckKey
  /** A starter's sphere, to name and colour it; null for one of your decks. */
  sphere: string | null
  name: string | null
  heroCodes: string[]
  detail: string | null
}

export const deckFaces = (decks: DeckSummary[]): DeckFace[] => [
  ...starterSpheres.map((sphere) => ({ key: `starter:${sphere}`, sphere, name: null, heroCodes: starterHeroes[sphere], detail: null })),
  ...decks.map((d) => ({ key: `deck:${d.id}`, sphere: null, name: d.name, heroCodes: d.heroes.map((h) => h.code), detail: d.heroes.map((h) => h.name).join(', ') })),
]
