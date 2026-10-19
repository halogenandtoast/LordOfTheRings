<script setup lang="ts">
/**
 * The card browser: products on the left, results in the middle, facets on the
 * right, and the selected card in a modal over the top.
 *
 * Filter state lives in the URL so a search can be linked to and survives a
 * reload.
 */
import '@browser/browser.css'
import { computed, onMounted, ref, watch } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { refDebounced } from '@vueuse/core'
import CardDialog from '@browser/components/CardDialog.vue'
import CardList from '@browser/components/CardList.vue'
import FilterSidebar from '@browser/components/FilterSidebar.vue'
import ProductBrowser from '@browser/components/ProductBrowser.vue'
import { useCardStore } from '@browser/stores/cards'
import { useUiStore } from '@browser/stores/ui'
import { SORT_LABELS, type SortField } from '@browser/lib/search'
import type { Card, CardType, Section, Sphere } from '@browser/types/card'

const store = useCardStore()
const ui = useUiStore()
const route = useRoute()
const router = useRouter()

/**
 * The search box writes here on every keystroke; the store — and so the
 * 4,000-card filter pass — only sees it once typing pauses.
 */
const searchInput = ref('')
const debouncedSearch = refDebounced(searchInput, 120)
watch(debouncedSearch, (value) => {
  store.filters.search = value
})

const selectedCode = ref<string | null>(null)

/**
 * Position of the selected card in the current results, which is what the
 * dialog's arrows walk. Derived rather than stored, so a card that filters out
 * from under the dialog closes it instead of stranding it.
 */
const selectedIndex = computed({
  get: () =>
    selectedCode.value ? store.results.findIndex((c) => c.code === selectedCode.value) : -1,
  set: (index: number) => {
    selectedCode.value = store.results[index]?.code ?? null
  },
})

function select(card: Card): void {
  selectedCode.value = card.code
}

function closeDialog(): void {
  selectedCode.value = null
}

// --- URL synchronisation ---------------------------------------------------

/** Repeated query params arrive as `string | string[]`; flatten to a list. */
function asList(value: unknown): string[] {
  if (Array.isArray(value)) return value.filter((v): v is string => typeof v === 'string')
  return typeof value === 'string' && value ? value.split(',').filter(Boolean) : []
}

function asNumber(value: unknown): number | null {
  const n = Number(value)
  return typeof value === 'string' && value !== '' && Number.isFinite(n) ? n : null
}

function readUrl(): void {
  const q = route.query
  searchInput.value = typeof q.q === 'string' ? q.q : ''
  store.filters.search = searchInput.value
  store.filters.types = asList(q.type) as CardType[]
  store.filters.spheres = asList(q.sphere) as Sphere[]
  store.filters.sections = asList(q.section) as Section[]
  store.filters.packs = asList(q.pack)
  store.filters.cycles = asList(q.cycle)
  store.filters.encounterSets = asList(q.set)
  store.filters.traits = asList(q.trait)
  store.filters.uniqueOnly = q.unique === '1'
  store.filters.costMin = asNumber(q.costMin)
  store.filters.costMax = asNumber(q.costMax)
  if (typeof q.sort === 'string' && q.sort in SORT_LABELS) store.sortField = q.sort as SortField
  store.sortDescending = q.desc === '1'
  selectedCode.value = typeof q.card === 'string' ? q.card : null
}

function writeUrl(): void {
  const q: Record<string, string> = {}
  const f = store.filters
  if (f.search) q.q = f.search
  if (f.types.length) q.type = f.types.join(',')
  if (f.spheres.length) q.sphere = f.spheres.join(',')
  if (f.sections.length) q.section = f.sections.join(',')
  if (f.packs.length) q.pack = f.packs.join(',')
  if (f.cycles.length) q.cycle = f.cycles.join(',')
  if (f.encounterSets.length) q.set = f.encounterSets.join(',')
  if (f.traits.length) q.trait = f.traits.join(',')
  if (f.uniqueOnly) q.unique = '1'
  if (f.costMin !== null) q.costMin = String(f.costMin)
  if (f.costMax !== null) q.costMax = String(f.costMax)
  if (store.sortField !== 'set') q.sort = store.sortField
  if (store.sortDescending) q.desc = '1'
  if (selectedCode.value) q.card = selectedCode.value

  // Filter changes replace rather than push: the back button should leave the
  // browser, not step back through every checkbox.
  router.replace({ query: q })
}

let syncing = false

onMounted(async () => {
  syncing = true
  readUrl()
  await store.load()
  syncing = false
})

watch(
  [() => store.filters, () => store.sortField, () => store.sortDescending, selectedCode],
  () => {
    if (!syncing) writeUrl()
  },
  { deep: true },
)

const sortOptions = Object.entries(SORT_LABELS) as [SortField, string][]

const resultLabel = computed(() => {
  const n = store.results.length
  const total = store.cards.length
  if (store.state !== 'ready') return ''
  if (n === total) return `${total.toLocaleString()} cards`
  return `${n.toLocaleString()} of ${total.toLocaleString()} cards`
})
</script>

<template>
  <div
    class="browse card-browser"
    :class="{ 'browse--products': ui.productsOpen, 'browse--filters': ui.filtersOpen }"
  >
    <ProductBrowser v-if="ui.productsOpen" class="browse__products" />

    <section class="browse__results">
      <div class="toolbar">
        <button
          type="button"
          class="button button--ghost"
          :aria-pressed="ui.productsOpen"
          :title="ui.productsOpen ? 'Hide products' : 'Show products'"
          @click="ui.productsOpen = !ui.productsOpen"
        >
          <span aria-hidden="true">&#9776;</span>
          <span class="visually-hidden">Toggle products</span>
        </button>

        <input
          v-model="searchInput"
          class="input toolbar__search"
          type="search"
          placeholder="Search names, traits and card text…"
          aria-label="Search cards"
          autocomplete="off"
        />

        <p class="toolbar__count" aria-live="polite">{{ resultLabel }}</p>

        <label class="toolbar__sort">
          <span class="visually-hidden">Sort by</span>
          <select v-model="store.sortField" class="input">
            <option v-for="[value, label] in sortOptions" :key="value" :value="value">
              {{ label }}
            </option>
          </select>
        </label>

        <button
          type="button"
          class="button button--ghost"
          :aria-pressed="store.sortDescending"
          :title="store.sortDescending ? 'Descending' : 'Ascending'"
          @click="store.sortDescending = !store.sortDescending"
        >
          <span aria-hidden="true">{{ store.sortDescending ? '↓' : '↑' }}</span>
          <span class="visually-hidden">Reverse sort order</span>
        </button>

        <div class="toolbar__views" role="group" aria-label="View mode">
          <button
            type="button"
            class="button"
            :aria-pressed="ui.viewMode === 'list'"
            @click="ui.viewMode = 'list'"
          >
            List
          </button>
          <button
            type="button"
            class="button"
            :aria-pressed="ui.viewMode === 'grid'"
            @click="ui.viewMode = 'grid'"
          >
            Grid
          </button>
        </div>

        <button
          type="button"
          class="button button--ghost toolbar__filters"
          :aria-pressed="ui.filtersOpen"
          :title="ui.filtersOpen ? 'Hide filters' : 'Show filters'"
          @click="ui.filtersOpen = !ui.filtersOpen"
        >
          <span aria-hidden="true">&#9662;</span>
          Filters
          <span v-if="store.filterCount" class="toolbar__badge">{{ store.filterCount }}</span>
        </button>
      </div>

      <div class="browse__body">
        <p v-if="store.state === 'loading'" class="notice">Loading the card database…</p>

        <div v-else-if="store.state === 'error'" class="notice notice--error">
          <p>Could not load the card database.</p>
          <p class="notice__detail">{{ store.error }}</p>
          <p class="notice__detail">Is the API running? <code>./dev.up</code></p>
        </div>

        <p v-else-if="!store.results.length" class="notice">
          No cards match those filters.
          <button
            v-if="store.hasFilters"
            type="button"
            class="button button--ghost"
            @click="store.resetFilters()"
          >
            Clear filters
          </button>
        </p>

        <CardList
          v-else
          :cards="store.results"
          :mode="ui.viewMode"
          :selected-code="selectedCode"
          @select="select"
        />
      </div>
    </section>

    <FilterSidebar v-if="ui.filtersOpen" class="browse__filters" />

    <CardDialog v-model:index="selectedIndex" :cards="store.results" @close="closeDialog" />
  </div>
</template>

<style scoped>
.browse {
  display: grid;
  grid-template-columns: minmax(0, 1fr);
  height: calc(100dvh - var(--header-height));
  overflow: hidden;
}

.browse--products {
  grid-template-columns: var(--products-width) minmax(0, 1fr);
}

.browse--filters {
  grid-template-columns: minmax(0, 1fr) var(--sidebar-width);
}

.browse--products.browse--filters {
  grid-template-columns: var(--products-width) minmax(0, 1fr) var(--sidebar-width);
}

.browse__products,
.browse__filters {
  min-height: 0;
}

.browse__results {
  display: flex;
  flex-direction: column;
  min-width: 0;
  min-height: 0;
}

.toolbar {
  display: flex;
  align-items: center;
  gap: var(--space-2);
  padding: var(--space-2) var(--space-3);
  border-bottom: 1px solid var(--c-border);
  background: var(--c-surface);
}

.toolbar__search {
  flex: 1;
  min-width: 8rem;
  max-width: 32rem;
}

.toolbar__count {
  color: var(--c-text-faint);
  font-size: var(--text-xs);
  font-variant-numeric: tabular-nums;
  white-space: nowrap;
}

.toolbar__sort .input {
  width: auto;
  padding-block: 0.3125rem;
}

.toolbar__views {
  display: flex;
  gap: 1px;
}

.toolbar__views .button:first-child {
  border-radius: var(--radius) 0 0 var(--radius);
}

.toolbar__views .button:last-child {
  border-radius: 0 var(--radius) var(--radius) 0;
}

.toolbar__badge {
  display: grid;
  place-items: center;
  min-width: 1.35em;
  height: 1.35em;
  padding: 0 0.35em;
  border-radius: 999px;
  background: var(--c-accent);
  color: var(--c-accent-contrast);
  font-size: 0.8em;
  font-weight: 600;
}

.browse__body {
  flex: 1;
  min-height: 0;
}

.notice {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: var(--space-3);
  padding: var(--space-6);
  color: var(--c-text-muted);
  text-align: center;
}

.notice--error {
  color: var(--c-danger);
}

.notice__detail {
  color: var(--c-text-faint);
  font-size: var(--text-sm);
}

.notice__detail code {
  font-family: var(--font-mono);
  font-size: 0.95em;
  background: var(--c-surface-2);
  padding: 0.1em 0.35em;
  border-radius: var(--radius-sm);
}

/*
 * Below this width there is not room for the results plus a side panel, so an
 * open panel floats over the results instead of taking a column from them.
 */
@media (width < 64rem) {
  .browse--products,
  .browse--filters,
  .browse--products.browse--filters {
    grid-template-columns: minmax(0, 1fr);
  }

  .browse__products,
  .browse__filters {
    position: fixed;
    top: var(--header-height);
    bottom: 0;
    z-index: 20;
    box-shadow: var(--shadow-lg);
  }

  .browse__products {
    left: 0;
    width: var(--products-width);
  }

  .browse__filters {
    right: 0;
    width: var(--sidebar-width);
  }

  .toolbar__sort,
  .toolbar__count {
    display: none;
  }
}
</style>
