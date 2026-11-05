/**
 * Decks and accounts, mirroring `backend/lotr-api/src/Lotr/Deck.hs` and
 * `Lotr/User.hs`. Keep the two in step: the Haskell side is authoritative.
 */
import type { Card } from './card'

export interface User {
  id: number
  username: string
  email: string
  createdAt: string
}

/**
 * One line of a deck list.
 *
 * `card` is null when the importer could not place `sourceCode` in our card
 * database — the slot is kept rather than dropped, so nothing a user imported
 * disappears without saying so.
 */
export interface Slot {
  quantity: number
  sourceCode: string
  cardCode: string | null
  card: Card | null
}

export interface Deck {
  id: number
  name: string
  description: string
  startingThreat: number | null
  /** `ringsdb` for a deck fetched by link, `json` for an uploaded file. */
  source: string
  sourceId: string | null
  sourceUrl: string | null
  createdAt: string
  updatedAt: string
  heroes: Slot[]
  /** The draw deck. Heroes are not in it. */
  slots: Slot[]
  sideslots: Slot[]
}

/** A deck without its cards: what the deck list shows. */
export interface DeckSummary {
  id: number
  name: string
  startingThreat: number | null
  source: string
  sourceUrl: string | null
  /** By RingsDB code, as the art is named by. */
  heroes: { code: string; name: string; sphere: string | null }[]
  cardCount: number
  unresolvedCount: number
  updatedAt: string
  /** When a game was last dealt from it. */
  usedAt: string | null
}

/** Exactly one of `url` and `deck`; `name` overrides the name in the source. */
export interface ImportRequest {
  url?: string
  deck?: unknown
  name?: string
  /** False to load the deck without keeping it, for the game at hand. */
  save?: boolean
}

/** Heroes and draw deck by RingsDB code, as the engine deals them. */
export interface DeckList {
  heroes: string[]
  cards: [string, number][]
  /** The contract chosen with the deck, never in it. */
  contract?: string | null
  /** Imported MotKA ally to prefer during Messenger of the King setup. */
  messengerOfTheKingAlly?: string | null
}

/** A deck loaded but not kept: what it holds, and its list, or why it cannot be played. */
export interface DeckPreview {
  deck: Omit<Deck, 'id' | 'createdAt' | 'updatedAt'>
  heroCodes: string[]
  cardCount: number
  list: DeckList | null
  problem: string | null
}

/** Every card in a deck, heroes and sideboard included. */
export function allSlots(deck: Deck): Slot[] {
  return [...deck.heroes, ...deck.slots, ...deck.sideslots]
}

export function unresolvedSlots(deck: Deck): Slot[] {
  return allSlots(deck).filter((slot) => slot.card === null)
}

/** Cards in the draw deck. The rules put the minimum at 50. A contract goes with the
 * deck but is not in it, nor counted toward its size. */
export function drawDeckSize(deck: Deck): number {
  return deck.slots.filter((slot) => slot.card?.type !== 'contract').reduce((n, slot) => n + slot.quantity, 0)
}
