<script setup lang="ts">
/**
 * The facet sidebar, on the right.
 *
 * Facets are ordered by how often they get used: section and type first,
 * sphere next, then the long tails — traits and encounter sets — which are
 * searchable and truncated. Which products to look at is a big enough question
 * to get its own panel; see ProductBrowser.
 */
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import FilterGroup from './FilterGroup.vue'
import GameIcon from './GameIcon.vue'
import { useCardStore } from '@browser/stores/cards'
import {
  ENCOUNTER_TYPES,
  PLAYER_SPHERES,
  PLAYER_TYPES,
  type CardType,
  type Section,
  type Sphere,
} from '@browser/types/card'
import type { IconName } from '@browser/lib/card-text'
import type { FilterOption } from '@browser/types/facets'

const store = useCardStore()
const { t, te } = useI18n()

const label = (group: string, value: string) =>
  te(`browser.${group}.${value}`) ? t(`browser.${group}.${value}`) : value

const sectionOptions = computed<FilterOption[]>(() =>
  (['player', 'encounter', 'quest', 'nightmare'] as Section[]).map((value) => ({
    value,
    label: t(`browser.sections.${value}`),
    count: store.facets.sections.find((f) => f.value === value)?.count,
  })),
)

/**
 * Types in game order rather than by frequency: a player looking for
 * "attachment" expects it next to "ally", not wherever the counts put it.
 */
const typeOptions = computed<FilterOption[]>(() => {
  const counts = new Map(store.facets.types.map((f) => [f.value, f.count]))
  const ordered = [...PLAYER_TYPES, ...ENCOUNTER_TYPES] as CardType[]
  const seen = new Set(ordered)
  const extras = store.facets.types.map((f) => f.value).filter((v) => !seen.has(v)) as CardType[]
  return [...ordered, ...extras]
    .filter((value) => counts.has(value))
    .map((value) => ({ value, label: label('cardTypes', value), count: counts.get(value) }))
})

const sphereOptions = computed<FilterOption[]>(() => {
  const counts = new Map(store.facets.spheres.map((f) => [f.value, f.count]))
  const ordered = [...PLAYER_SPHERES] as Sphere[]
  const extras = store.facets.spheres
    .map((f) => f.value)
    .filter((v) => !ordered.includes(v)) as Sphere[]
  return [...ordered, ...extras]
    .filter((value) => counts.has(value))
    .map((value) => ({ value, label: label('spheres', value), count: counts.get(value) }))
})

const traitOptions = computed<FilterOption[]>(() =>
  store.facets.traits.map((f) => ({ value: f.value, label: f.value, count: f.count })),
)

const encounterSetOptions = computed<FilterOption[]>(() =>
  store.facets.encounterSets.map((f) => ({ value: f.value, label: f.value, count: f.count })),
)

/**
 * `v-model.number` leaves an empty input as `''`, which is neither a bound nor
 * an absent one. These proxies keep the store holding `number | null`.
 */
function costBound(key: 'costMin' | 'costMax') {
  return computed({
    get: () => store.filters[key],
    set: (value: number | string | null) => {
      store.filters[key] = typeof value === 'number' && Number.isFinite(value) ? value : null
    },
  })
}

const costMin = costBound('costMin')
const costMax = costBound('costMax')

const SPHERE_ICONS = new Set<string>(PLAYER_SPHERES)
function sphereIcon(value: string): IconName | null {
  return SPHERE_ICONS.has(value) ? (value as IconName) : null
}
</script>

<template>
  <aside class="sidebar" :aria-label="$t('browser.filters.title')">
    <div class="sidebar__head">
      <h2 class="sidebar__title">{{ $t('browser.filters.title') }}</h2>
      <button
        v-if="store.filterCount"
        type="button"
        class="button button--ghost sidebar__clear"
        @click="store.resetFacets()"
      >
        {{ $t('browser.common.clearCount', { count: store.filterCount }) }}
      </button>
    </div>

    <FilterGroup
      :title="$t('browser.filters.section')"
      :options="sectionOptions"
      :selected="store.filters.sections"
      @toggle="store.toggle('sections', $event as Section)"
    />

    <FilterGroup
      :title="$t('browser.filters.type')"
      :options="typeOptions"
      :selected="store.filters.types"
      :max-visible="8"
      @toggle="store.toggle('types', $event as CardType)"
    />

    <FilterGroup
      :title="$t('browser.filters.sphere')"
      :options="sphereOptions"
      :selected="store.filters.spheres"
      @toggle="store.toggle('spheres', $event as Sphere)"
    >
      <template #option="{ option }">
        <GameIcon v-if="sphereIcon(option.value)" :name="sphereIcon(option.value)!" />
        {{ option.label }}
      </template>
    </FilterGroup>

    <section class="group group--cost">
      <h3 class="group__title">{{ $t('browser.filters.cost') }}</h3>
      <div class="cost-range">
        <label>
          <span class="visually-hidden">{{ $t('browser.filters.minimumCost') }}</span>
          <input
            v-model.number="costMin"
            class="input"
            type="number"
            min="0"
            max="99"
            :placeholder="$t('browser.filters.min')"
          />
        </label>
        <span aria-hidden="true">–</span>
        <label>
          <span class="visually-hidden">{{ $t('browser.filters.maximumCost') }}</span>
          <input
            v-model.number="costMax"
            class="input"
            type="number"
            min="0"
            max="99"
            :placeholder="$t('browser.filters.max')"
          />
        </label>
      </div>
      <label class="unique-toggle">
        <input v-model="store.filters.uniqueOnly" type="checkbox" />
        {{ $t('browser.filters.uniqueOnly') }}
      </label>
    </section>

    <FilterGroup
      :title="$t('browser.filters.traits')"
      :options="traitOptions"
      :selected="store.filters.traits"
      searchable
      :search-placeholder="$t('browser.filters.searchTraits')"
      :max-visible="10"
      :open="false"
      @toggle="store.toggle('traits', $event)"
    />

    <FilterGroup
      :title="$t('browser.filters.encounterSet')"
      :options="encounterSetOptions"
      :selected="store.filters.encounterSets"
      searchable
      :search-placeholder="$t('browser.filters.searchEncounterSets')"
      :max-visible="10"
      :open="false"
      @toggle="store.toggle('encounterSets', $event)"
    />
  </aside>
</template>

<style scoped>
.sidebar {
  display: flex;
  flex-direction: column;
  height: 100%;
  overflow-y: auto;
  scrollbar-gutter: stable;
  overscroll-behavior: contain;
  background: var(--c-surface);
  border-left: 1px solid var(--c-border);
}

.sidebar__head {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: var(--space-2);
  padding: var(--space-3) var(--space-4);
  border-bottom: 1px solid var(--c-border);
  position: sticky;
  top: 0;
  background: var(--c-surface);
  z-index: 1;
}

.sidebar__title {
  font-size: var(--text-sm);
}

.sidebar__clear {
  font-size: var(--text-xs);
  padding: 0.125rem 0.5rem;
  color: var(--c-accent);
}

.group--cost {
  padding: var(--space-3) var(--space-4);
  border-bottom: 1px solid var(--c-border);
}

.group__title {
  margin-bottom: var(--space-2);
  color: var(--c-text-muted);
  font-size: var(--text-xs);
  font-weight: 600;
  letter-spacing: 0.06em;
  text-transform: uppercase;
}

.cost-range {
  display: grid;
  grid-template-columns: 1fr auto 1fr;
  align-items: center;
  gap: var(--space-2);
  color: var(--c-text-faint);
}

.cost-range .input {
  text-align: center;
}

/* Number spinners crowd a 4rem input to no benefit. */
.cost-range input[type='number']::-webkit-inner-spin-button,
.cost-range input[type='number']::-webkit-outer-spin-button {
  appearance: none;
  margin: 0;
}

.cost-range input[type='number'] {
  appearance: textfield;
}

.unique-toggle {
  display: flex;
  align-items: center;
  gap: var(--space-2);
  margin-top: var(--space-3);
  font-size: var(--text-sm);
  cursor: pointer;
}

.unique-toggle input {
  accent-color: var(--c-accent);
}
</style>
