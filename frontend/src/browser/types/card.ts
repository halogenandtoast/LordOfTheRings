/**
 * The card model, mirroring `backend/src/Lotr/Card.hs` and the JSON the
 * importer writes. Keep the two in step: the Haskell side is authoritative.
 */

export type Section = 'player' | 'encounter' | 'quest' | 'nightmare'

export type Orientation = 'portrait' | 'landscape'

export type CardType =
  | 'hero'
  | 'ally'
  | 'attachment'
  | 'event'
  | 'player-side-quest'
  | 'contract'
  | 'treasure'
  | 'enemy'
  | 'ship-enemy'
  | 'location'
  | 'treachery'
  | 'objective'
  | 'objective-ally'
  | 'side-quest'
  | 'quest'
  | 'campaign'
  | 'nightmare'

export type Sphere =
  | 'leadership'
  | 'lore'
  | 'spirit'
  | 'tactics'
  | 'baggins'
  | 'fellowship'
  | 'neutral'
  | 'boon'
  | 'burden'
  | 'special'

/** A printed stat: a number, or the variable `X` the card text defines. */
export type Stat = number | 'X'

export interface CardImage {
  /**
   * The side letter printed on the card. Usually `a` or `b`, but a quest
   * stage printed in several variants keeps counting rather than restarting,
   * so these run as far as `h`.
   */
  face: string
  variant: string | null
  stage: number | null
  orientation: Orientation
  /** Path of the original scan. Provenance only; not served. */
  source: string
  /** File name under `/images/browse` and `/images/browse/thumb`, from the client's public/. */
  file: string
}

export interface CardBack {
  name: string | null
  type: CardType | null
  traits: string[]
  text: string | null
  cost: Stat | null
  threatCost: Stat | null
  willpower: Stat | null
  attack: Stat | null
  defense: Stat | null
  health: Stat | null
  threat: Stat | null
  engagementCost: Stat | null
  questPoints: Stat | null
  victoryPoints: Stat | null
  stage: number | null
}

export interface Card {
  code: string
  name: string
  section: Section
  number: number | null
  /** Numbering scope: the pack, or one nightmare deck. */
  setCode: string
  setName: string
  packCode: string
  packName: string
  cycleCode: string
  cycleName: string
  encounterSet: string | null

  type: CardType | null
  typeName: string | null
  sphere: Sphere | null
  traits: string[]
  keywords: string[]
  unique: boolean

  cost: Stat | null
  threatCost: Stat | null
  willpower: Stat | null
  attack: Stat | null
  defense: Stat | null
  health: Stat | null
  threat: Stat | null
  engagementCost: Stat | null
  questPoints: Stat | null
  victoryPoints: Stat | null
  stage: number | null

  text: string | null
  shadow: string | null
  setupText: string | null
  flavor: string | null

  quantity: number | null
  deckLimit: number | null
  illustrator: string | null
  hasErrata: boolean

  back: CardBack | null
  /** Set when this card is the reverse of a differently-named front face. */
  isBackOf: string | null
  octgnId: string | null
  ringsdbCode: string | null
  ringsdbUrl: string | null

  orientation: Orientation
  images: CardImage[]
}

export interface Pack {
  code: string
  name: string
  order: number | null
  cycleCode: string
  cycleName: string
  cycleOrder: number | null
  cardCount: number
}

export interface Cycle {
  code: string
  name: string
  order: number | null
  packCodes: string[]
}

export interface CardIndex {
  version: number
  generatedAt: string
  cycles: Cycle[]
  packs: Pack[]
  cards: Card[]
}

/** Player spheres, in the order the game presents them. */
export const PLAYER_SPHERES = [
  'leadership',
  'lore',
  'spirit',
  'tactics',
  'baggins',
  'fellowship',
  'neutral',
] as const satisfies readonly Sphere[]

/** Types a player can put in a deck, in the order the deckbuilder shows them. */
export const PLAYER_TYPES = [
  'hero',
  'ally',
  'attachment',
  'event',
  'player-side-quest',
  'contract',
  'treasure',
] as const satisfies readonly CardType[]

export const ENCOUNTER_TYPES = [
  'enemy',
  'location',
  'treachery',
  'objective',
  'objective-ally',
  'ship-enemy',
  'side-quest',
  'quest',
  'campaign',
  'nightmare',
] as const satisfies readonly CardType[]

/** The primary cost shown on a card: heroes are priced in threat. */
export function primaryCost(card: Card): Stat | null {
  return card.type === 'hero' ? card.threatCost : card.cost
}

export function statNumber(stat: Stat | null | undefined): number | null {
  return typeof stat === 'number' ? stat : null
}

export function imageUrl(card: Card, size: 'full' | 'thumb' = 'full'): string | null {
  const image = card.images[0]
  if (!image) return null
  return size === 'thumb' ? `/images/browse/thumb/${image.file}` : `/images/browse/${image.file}`
}

export function imageUrlFor(image: CardImage, size: 'full' | 'thumb' = 'full'): string {
  return size === 'thumb' ? `/images/browse/thumb/${image.file}` : `/images/browse/${image.file}`
}
