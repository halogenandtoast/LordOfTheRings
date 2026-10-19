<script setup lang="ts">
/**
 * The product browser: every cycle, and the packs inside it.
 *
 * This is the "which boxes am I looking at" question, which is asked often
 * enough and has enough structure to deserve its own panel rather than a row
 * in the facet list. It drives `filters.packs` alone — a cycle checkbox is
 * shorthand for all of its packs — so the selection has one representation
 * and the URL stays readable.
 */
import { computed, ref } from 'vue'
import { useCardStore } from '@browser/stores/cards'
import { useUiStore } from '@browser/stores/ui'
import { normalize } from '@browser/lib/search'
import type { Pack } from '@browser/types/card'

const store = useCardStore()
const ui = useUiStore()

const query = ref('')

interface CycleRow {
  code: string
  name: string
  packs: Pack[]
  cardCount: number
  /** How many of this cycle's packs are selected. */
  selected: number
}

const cycles = computed<CycleRow[]>(() => {
  const needle = normalize(query.value)
  return store.cyclesWithPacks
    .map((cycle) => {
      // A search matches on the cycle name too, so typing "hobbit" keeps the
      // whole box rather than only the packs whose own names contain it.
      const cycleMatches = needle !== '' && normalize(cycle.name).includes(needle)
      const packs =
        needle === '' || cycleMatches
          ? cycle.packs
          : cycle.packs.filter((p) => normalize(p.name).includes(needle))
      return {
        code: cycle.code,
        name: cycle.name,
        packs,
        cardCount: packs.reduce((n, p) => n + p.cardCount, 0),
        selected: packs.filter((p) => store.filters.packs.includes(p.code)).length,
      }
    })
    .filter((cycle) => cycle.packs.length > 0)
})

const selectedCount = computed(() => store.filters.packs.length)

/** A cycle whose only pack shares its name is a box, not a cycle of packs. */
function isSingleton(cycle: CycleRow): boolean {
  return cycle.packs.length === 1 && normalize(cycle.packs[0]!.name) === normalize(cycle.name)
}

/** Searching should reveal what it found, without disturbing the saved state. */
function expanded(cycle: CycleRow): boolean {
  return query.value !== '' || ui.isCycleExpanded(cycle.code)
}

function toggleCycle(cycle: CycleRow): void {
  const codes = cycle.packs.map((p) => p.code)
  const chosen = store.filters.packs
  // Partial selections fill in rather than clear: clicking a half-checked box
  // to get the whole cycle is the common intent.
  if (cycle.selected === codes.length) {
    store.filters.packs = chosen.filter((code) => !codes.includes(code))
  } else {
    store.filters.packs = [...new Set([...chosen, ...codes])]
  }
}

function togglePack(code: string): void {
  store.toggle('packs', code)
}

function isPackSelected(code: string): boolean {
  return store.filters.packs.includes(code)
}
</script>

<template>
  <nav class="products" aria-label="Products">
    <div class="products__head">
      <h2 class="products__title">Products</h2>
      <button
        v-if="selectedCount"
        type="button"
        class="button button--ghost products__clear"
        @click="store.filters.packs = []"
      >
        Clear {{ selectedCount }}
      </button>
    </div>

    <div class="products__search">
      <input
        v-model="query"
        class="input"
        type="search"
        placeholder="Find a set…"
        aria-label="Search products"
      />
    </div>

    <ul class="tree">
      <li v-for="cycle in cycles" :key="cycle.code" class="cycle">
        <div class="cycle__row" :class="{ 'cycle__row--on': cycle.selected > 0 }">
          <button
            v-if="!isSingleton(cycle)"
            type="button"
            class="cycle__toggle"
            :aria-expanded="expanded(cycle)"
            :title="expanded(cycle) ? 'Collapse' : 'Expand'"
            @click="ui.toggleCycle(cycle.code)"
          >
            <span class="chevron" :class="{ 'chevron--open': expanded(cycle) }" aria-hidden="true">
              &#9656;
            </span>
            <span class="visually-hidden">{{ cycle.name }}</span>
          </button>
          <span v-else class="cycle__toggle cycle__toggle--empty" aria-hidden="true" />

          <label class="cycle__label">
            <input
              type="checkbox"
              :checked="cycle.selected === cycle.packs.length"
              :indeterminate.prop="cycle.selected > 0 && cycle.selected < cycle.packs.length"
              @change="toggleCycle(cycle)"
            />
            <span class="cycle__name truncate">{{ cycle.name }}</span>
            <span class="count">{{ cycle.cardCount }}</span>
          </label>
        </div>

        <ul v-if="expanded(cycle) && !isSingleton(cycle)" class="packs">
          <li v-for="pack in cycle.packs" :key="pack.code">
            <label class="pack" :class="{ 'pack--on': isPackSelected(pack.code) }">
              <input
                type="checkbox"
                :checked="isPackSelected(pack.code)"
                @change="togglePack(pack.code)"
              />
              <span class="truncate">{{ pack.name }}</span>
              <span class="count">{{ pack.cardCount }}</span>
            </label>
          </li>
        </ul>
      </li>

      <li v-if="!cycles.length" class="tree__empty">No products match “{{ query }}”.</li>
    </ul>
  </nav>
</template>

<style scoped>
.products {
  display: flex;
  flex-direction: column;
  height: 100%;
  min-height: 0;
  background: var(--c-surface);
  border-right: 1px solid var(--c-border);
}

.products__head {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: var(--space-2);
  padding: var(--space-3) var(--space-4);
  border-bottom: 1px solid var(--c-border);
}

.products__title {
  font-size: var(--text-sm);
}

.products__clear {
  padding: 0.125rem 0.5rem;
  color: var(--c-accent);
  font-size: var(--text-xs);
}

.products__search {
  padding: var(--space-3) var(--space-3) var(--space-2);
}

.tree {
  flex: 1;
  min-height: 0;
  overflow-y: auto;
  overscroll-behavior: contain;
  padding: 0 var(--space-2) var(--space-4);
}

.cycle__row {
  display: flex;
  align-items: center;
  border-radius: var(--radius-sm);
}

.cycle__row:hover {
  background: var(--c-surface-2);
}

.cycle__row--on .cycle__name {
  color: var(--c-accent);
}

.cycle__toggle {
  display: grid;
  place-items: center;
  width: 1.5rem;
  height: 1.75rem;
  flex: none;
  color: var(--c-text-faint);
}

.cycle__toggle:hover {
  color: var(--c-text);
}

.chevron {
  font-size: 0.8em;
  transition: transform var(--transition);
}

.chevron--open {
  transform: rotate(90deg);
}

.cycle__label,
.pack {
  display: flex;
  align-items: center;
  gap: var(--space-2);
  flex: 1;
  min-width: 0;
  padding: 0.25rem var(--space-2) 0.25rem 0;
  font-size: var(--text-sm);
  cursor: pointer;
}

.cycle__label {
  font-weight: 500;
}

.cycle__name {
  flex: 1;
  min-width: 0;
}

.packs {
  /* Indented to the cycle's checkbox, with a rule tying the group together. */
  margin-left: 1.5rem;
  padding-left: var(--space-3);
  border-left: 1px solid var(--c-border);
}

.pack {
  padding-left: var(--space-1);
  border-radius: var(--radius-sm);
  color: var(--c-text-muted);
}

.pack:hover {
  background: var(--c-surface-2);
  color: var(--c-text);
}

.pack--on {
  color: var(--c-accent);
}

.pack span:not(.count),
.cycle__name {
  flex: 1;
  min-width: 0;
}

input[type='checkbox'] {
  accent-color: var(--c-accent);
  flex: none;
  margin: 0;
}

.count {
  flex: none;
  color: var(--c-text-faint);
  font-size: var(--text-xs);
  font-variant-numeric: tabular-nums;
}

.tree__empty {
  padding: var(--space-3);
  color: var(--c-text-faint);
  font-size: var(--text-sm);
}
</style>
