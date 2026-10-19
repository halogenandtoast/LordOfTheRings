/**
 * The card database and the browse state that reads from it.
 *
 * The index is fetched once and never refetched; everything the browser does
 * after that is a derived computation over an in-memory array.
 */
import { computed, ref, shallowRef } from 'vue'
import { defineStore } from './store'
import {
  activeFilterCount,
  clearFacets,
  emptyFilters,
  filterCards,
  isEmptyFilters,
  sortCards,
  type CardFilters,
  type SortField,
} from '@browser/lib/search'
import type { Card, CardIndex, CardType, Cycle, Pack, Section, Sphere } from '@browser/types/card'

// ponytail: the database is a static file from the importer, served as is; an API
// endpoint earns its keep only once the server needs to read the cards itself.
async function fetchCardIndex(): Promise<CardIndex> {
  const response = await fetch('/data/cards.json')
  if (!response.ok) throw new Error(`Could not load the card database (${response.status})`)
  return response.json()
}

export type LoadState = 'idle' | 'loading' | 'ready' | 'error'

export const useCardStore = defineStore('cards', () => {
  // shallowRef: the card array is large and immutable once loaded, so deep
  // reactivity would cost a full proxy walk for nothing.
  const cards = shallowRef<Card[]>([])
  const packs = shallowRef<Pack[]>([])
  const cycles = shallowRef<Cycle[]>([])
  const generatedAt = ref('')

  const state = ref<LoadState>('idle')
  const error = ref<string | null>(null)

  const filters = ref<CardFilters>(emptyFilters())
  const sortField = ref<SortField>('set')
  const sortDescending = ref(false)

  let inFlight: Promise<void> | null = null

  async function load(): Promise<void> {
    if (state.value === 'ready') return
    if (inFlight) return inFlight

    state.value = 'loading'
    error.value = null
    inFlight = fetchCardIndex()
      .then((index) => {
        cards.value = index.cards
        packs.value = index.packs
        cycles.value = index.cycles
        generatedAt.value = index.generatedAt
        state.value = 'ready'
      })
      .catch((err: unknown) => {
        error.value = err instanceof Error ? err.message : String(err)
        state.value = 'error'
      })
      .finally(() => {
        inFlight = null
      })

    return inFlight
  }

  const byCode = computed(() => new Map(cards.value.map((c) => [c.code, c])))

  function cardByCode(code: string): Card | undefined {
    return byCode.value.get(code)
  }

  const packByCode = computed(() => new Map(packs.value.map((p) => [p.code, p])))

  /** Packs grouped under their cycle, in release order. */
  const cyclesWithPacks = computed(() =>
    cycles.value.map((cycle) => ({
      ...cycle,
      packs: cycle.packCodes
        .map((code) => packByCode.value.get(code))
        .filter((p): p is Pack => p !== undefined)
        .sort((a, b) => (a.order ?? 0) - (b.order ?? 0)),
    })),
  )

  const matched = computed(() => filterCards(cards.value, filters.value))

  const results = computed(() => sortCards(matched.value, sortField.value, sortDescending.value))

  /**
   * Facet values present in the database, with how many cards carry each.
   *
   * Counted over everything rather than over the current results: a facet that
   * vanishes as soon as you use a neighbouring one is disorienting.
   */
  const facets = computed(() => {
    const types = new Map<CardType, number>()
    const spheres = new Map<Sphere, number>()
    const sections = new Map<Section, number>()
    const traits = new Map<string, number>()
    const encounterSets = new Map<string, number>()

    for (const card of cards.value) {
      if (card.type) types.set(card.type, (types.get(card.type) ?? 0) + 1)
      if (card.sphere) spheres.set(card.sphere, (spheres.get(card.sphere) ?? 0) + 1)
      sections.set(card.section, (sections.get(card.section) ?? 0) + 1)
      for (const trait of card.traits) traits.set(trait, (traits.get(trait) ?? 0) + 1)
      if (card.encounterSet)
        encounterSets.set(card.encounterSet, (encounterSets.get(card.encounterSet) ?? 0) + 1)
    }

    const byCountThenName = <K extends string>(entries: Map<K, number>) =>
      [...entries.entries()]
        .sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))
        .map(([value, count]) => ({ value, count }))

    return {
      types: byCountThenName(types),
      spheres: byCountThenName(spheres),
      sections: byCountThenName(sections),
      traits: byCountThenName(traits),
      encounterSets: byCountThenName(encounterSets),
    }
  })

  const hasFilters = computed(() => !isEmptyFilters(filters.value))

  /** Facets only; the product browser keeps its own count. */
  const filterCount = computed(() => activeFilterCount(filters.value))

  /** Clear everything, products included. */
  function resetFilters(): void {
    filters.value = emptyFilters()
  }

  /** Clear the facets but keep the chosen products. */
  function resetFacets(): void {
    filters.value = clearFacets(filters.value)
  }

  /** Add or remove one value from a multi-select facet. */
  function toggle<K extends keyof CardFilters>(
    key: K,
    value: CardFilters[K] extends (infer T)[] ? T : never,
  ): void {
    const list = filters.value[key] as unknown[]
    const index = list.indexOf(value)
    if (index === -1) list.push(value)
    else list.splice(index, 1)
  }

  return {
    cards,
    packs,
    cycles,
    generatedAt,
    state,
    error,
    filters,
    sortField,
    sortDescending,
    load,
    cardByCode,
    cyclesWithPacks,
    packByCode,
    matched,
    results,
    facets,
    hasFilters,
    filterCount,
    resetFilters,
    resetFacets,
    toggle,
  }
})
