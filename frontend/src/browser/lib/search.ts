/**
 * Card searching and filtering.
 *
 * The whole database is 4100 cards, so filtering happens in the browser: no
 * round trip, no loading state between keystrokes. The server exposes the same
 * predicates for other clients (see `Lotr.Card.Query`).
 */
import type { Card, CardType, Section, Sphere } from '@browser/types/card'
import { primaryCost, statNumber } from '@browser/types/card'

/**
 * Fold case, strip accents and drop punctuation, so a plain keyboard finds
 * `Sméagol` and `Ungoliant's Spawn`.
 */
export function normalize(text: string): string {
  return text
    .normalize('NFKD')
    .replace(/[̀-ͯ]/g, '')
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '')
}

export interface CardFilters {
  search: string
  types: CardType[]
  spheres: Sphere[]
  sections: Section[]
  packs: string[]
  cycles: string[]
  encounterSets: string[]
  traits: string[]
  uniqueOnly: boolean
  costMin: number | null
  costMax: number | null
}

export function emptyFilters(): CardFilters {
  return {
    search: '',
    types: [],
    spheres: [],
    sections: [],
    packs: [],
    cycles: [],
    encounterSets: [],
    traits: [],
    uniqueOnly: false,
    costMin: null,
    costMax: null,
  }
}

export function isEmptyFilters(f: CardFilters): boolean {
  return (
    f.search.trim() === '' &&
    f.types.length === 0 &&
    f.spheres.length === 0 &&
    f.sections.length === 0 &&
    f.packs.length === 0 &&
    f.cycles.length === 0 &&
    f.encounterSets.length === 0 &&
    f.traits.length === 0 &&
    !f.uniqueOnly &&
    f.costMin === null &&
    f.costMax === null
  )
}

/**
 * How many facets are active, for the badge on the Filters panel.
 *
 * Products are excluded: they live in their own panel, which keeps its own
 * count, and having both claim the same selection reads as double-counting.
 */
export function activeFilterCount(f: CardFilters): number {
  return (
    f.types.length +
    f.spheres.length +
    f.sections.length +
    f.encounterSets.length +
    f.traits.length +
    (f.uniqueOnly ? 1 : 0) +
    (f.costMin !== null || f.costMax !== null ? 1 : 0)
  )
}

/** Everything the Filters panel owns, leaving the product selection alone. */
export function clearFacets(f: CardFilters): CardFilters {
  return { ...emptyFilters(), packs: f.packs, cycles: f.cycles }
}

/**
 * A card's searchable text, normalised once and cached on the card.
 *
 * Recomputing this per keystroke across 4100 cards is the difference between a
 * responsive search box and a janky one, so the result is memoised in a
 * WeakMap keyed by the card object.
 */
const haystacks = new WeakMap<Card, string>()

function haystack(card: Card): string {
  let value = haystacks.get(card)
  if (value === undefined) {
    value = normalize(
      [
        card.name,
        ...card.traits,
        ...card.keywords,
        card.text ?? '',
        card.shadow ?? '',
        card.flavor ?? '',
        card.encounterSet ?? '',
        card.back?.name ?? '',
        card.back?.text ?? '',
      ].join(' '),
    )
    haystacks.set(card, value)
  }
  return value
}

/** Terms are conjunctive: `dwarf warrior` wants both, in any field. */
function matchesSearch(card: Card, terms: string[]): boolean {
  if (terms.length === 0) return true
  const text = haystack(card)
  return terms.every((term) => text.includes(term))
}

export function searchTerms(query: string): string[] {
  return query
    .split(/\s+/)
    .map(normalize)
    .filter((t) => t.length > 0)
}

export function filterCards(cards: Card[], filters: CardFilters): Card[] {
  const terms = searchTerms(filters.search)
  const traits = filters.traits.map(normalize)
  const hasCostBound = filters.costMin !== null || filters.costMax !== null

  return cards.filter((card) => {
    if (!matchesSearch(card, terms)) return false
    if (filters.types.length && (!card.type || !filters.types.includes(card.type))) return false
    if (filters.spheres.length && (!card.sphere || !filters.spheres.includes(card.sphere)))
      return false
    if (filters.sections.length && !filters.sections.includes(card.section)) return false
    if (filters.packs.length && !filters.packs.includes(card.packCode)) return false
    if (filters.cycles.length && !filters.cycles.includes(card.cycleCode)) return false
    if (
      filters.encounterSets.length &&
      (!card.encounterSet || !filters.encounterSets.includes(card.encounterSet))
    )
      return false
    if (filters.uniqueOnly && !card.unique) return false

    // Traits are conjunctive too: a Noble Ranger filter wants both.
    if (traits.length) {
      const own = card.traits.map(normalize)
      if (!traits.every((t) => own.includes(t))) return false
    }

    if (hasCostBound) {
      const cost = statNumber(primaryCost(card))
      if (cost === null) return false
      if (filters.costMin !== null && cost < filters.costMin) return false
      if (filters.costMax !== null && cost > filters.costMax) return false
    }

    return true
  })
}

export type SortField = 'set' | 'name' | 'cost' | 'type' | 'sphere'

export const SORT_FIELDS: SortField[] = ['set', 'name', 'cost', 'type', 'sphere']

/** Cards with no value for the sort key go last, whichever way we sort. */
function nullsLast<T>(value: T | null): [number, T | null] {
  return [value === null ? 1 : 0, value]
}

export function sortCards(cards: Card[], field: SortField, descending = false): Card[] {
  if (field === 'set' && !descending) return cards

  const sorted = [...cards]
  const byName = (a: Card, b: Card) => normalize(a.name).localeCompare(normalize(b.name))

  sorted.sort((a, b) => {
    switch (field) {
      case 'set':
        return 0
      case 'name':
        return byName(a, b)
      case 'cost': {
        const [an, av] = nullsLast(statNumber(primaryCost(a)))
        const [bn, bv] = nullsLast(statNumber(primaryCost(b)))
        return an - bn || (av ?? 0) - (bv ?? 0) || byName(a, b)
      }
      case 'type': {
        const [an, av] = nullsLast(a.type)
        const [bn, bv] = nullsLast(b.type)
        return an - bn || (av ?? '').localeCompare(bv ?? '') || byName(a, b)
      }
      case 'sphere': {
        const [an, av] = nullsLast(a.sphere)
        const [bn, bv] = nullsLast(b.sphere)
        return an - bn || (av ?? '').localeCompare(bv ?? '') || byName(a, b)
      }
    }
  })

  return descending ? sorted.reverse() : sorted
}
