<script setup lang="ts">
/**
 * One card in the dense list view: thumbnail, name, cost, type, traits, stats.
 *
 * Rows are fixed height so the virtual list can position them without
 * measuring, which is what keeps 4000 of them scrolling smoothly.
 */
import { computed } from 'vue'
import CardImage from './CardImage.vue'
import CardStats from './CardStats.vue'
import GameIcon from './GameIcon.vue'
import type { Card } from '@browser/types/card'
import { primaryCost, TYPE_LABELS } from '@browser/types/card'
import type { IconName } from '@browser/lib/card-text'

const { card, selected = false } = defineProps<{ card: Card; selected?: boolean }>()

const cost = computed(() => primaryCost(card))
const costLabel = computed(() => (card.type === 'hero' ? 'Threat cost' : 'Resource cost'))
const typeLabel = computed(() => (card.type ? TYPE_LABELS[card.type] : (card.typeName ?? '—')))

/** Only the four player spheres have a glyph; the rest show as a colour dot. */
const sphereIcon = computed<IconName | null>(() => {
  const sphere = card.sphere
  if (!sphere) return null
  return (
    ['leadership', 'lore', 'spirit', 'tactics', 'baggins', 'fellowship', 'neutral'] as const
  ).includes(sphere as never)
    ? (sphere as IconName)
    : null
})
</script>

<template>
  <article
    class="row"
    :class="{ 'row--selected': selected }"
    :data-sphere="card.sphere ?? undefined"
    :data-type="card.type ?? undefined"
  >
    <div class="row__thumb">
      <CardImage :card="card" size="thumb" tight />
    </div>

    <div class="row__main">
      <h3 class="row__name">
        <span v-if="card.unique" class="unique" aria-label="Unique">&#9670;</span>
        {{ card.name }}
        <span v-if="card.hasErrata" class="errata" title="Has errata">e</span>
      </h3>

      <p v-if="card.traits.length" class="row__traits">{{ card.traits.join('. ') }}.</p>
      <p v-else-if="card.encounterSet" class="row__traits row__traits--set">
        {{ card.encounterSet }}
      </p>
    </div>

    <div class="row__cost">
      <template v-if="cost !== null">
        <span class="cost" :title="costLabel">{{ cost }}</span>
        <GameIcon v-if="sphereIcon" :name="sphereIcon" />
        <span v-else-if="card.sphere" class="sphere-dot" :title="card.sphere" />
      </template>
      <GameIcon v-else-if="sphereIcon" :name="sphereIcon" />
    </div>

    <div class="row__type">
      <span class="type-dot" />
      {{ typeLabel }}
    </div>

    <div class="row__stats">
      <CardStats :card="card" compact />
    </div>

    <div class="row__set">
      <span class="truncate">{{ card.packName }}</span>
      <span v-if="card.number !== null" class="row__number">
        {{ String(card.number).padStart(3, '0') }}
      </span>
    </div>
  </article>
</template>

<style scoped>
.row {
  display: grid;
  grid-template-columns: 2.25rem minmax(9rem, 1.6fr) 3.5rem 7rem minmax(6rem, 1fr) 11rem;
  align-items: center;
  gap: var(--space-3);
  height: 100%;
  padding: 0 var(--space-3);
  border-bottom: 1px solid var(--c-border);
  transition: background var(--transition);
}

.row:hover {
  background: var(--c-surface-2);
}

.row--selected {
  background: var(--c-surface-3);
  box-shadow: inset 3px 0 0 var(--c-accent);
}

.row__thumb {
  width: 2.25rem;
}

.row__main {
  min-width: 0;
}

.row__name {
  display: flex;
  align-items: baseline;
  gap: 0.3em;
  font-size: var(--text-base);
  font-weight: 500;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

/* The unique bullet is printed in the sphere's colour on the card itself. */
.unique {
  color: var(--sphere-color, var(--c-accent));
  font-size: 0.7em;
  line-height: 1;
}

.errata {
  display: inline-grid;
  place-items: center;
  width: 1.05em;
  height: 1.05em;
  border-radius: 50%;
  background: var(--c-surface-3);
  color: var(--c-text-faint);
  font-size: 0.65em;
  font-weight: 700;
  cursor: help;
}

.row__traits {
  font-size: var(--text-xs);
  color: var(--c-text-faint);
  font-style: italic;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.row__traits--set {
  font-style: normal;
}

.row__cost {
  display: flex;
  align-items: center;
  justify-content: flex-end;
  gap: 0.25em;
  font-variant-numeric: tabular-nums;
}

.cost {
  font-weight: 600;
}

.sphere-dot {
  width: 0.6em;
  height: 0.6em;
  border-radius: 50%;
  background: var(--sphere-color, var(--c-text-faint));
}

.row__type {
  display: flex;
  align-items: center;
  gap: var(--space-2);
  font-size: var(--text-sm);
  color: var(--c-text-muted);
}

.type-dot {
  width: 0.5em;
  height: 0.5em;
  border-radius: 50%;
  background: var(--type-color, var(--sphere-color, var(--c-border-strong)));
  flex: none;
}

.row__set {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  gap: var(--space-2);
  min-width: 0;
  font-size: var(--text-xs);
  color: var(--c-text-faint);
}

.row__number {
  font-variant-numeric: tabular-nums;
  flex: none;
}

@media (width < 60rem) {
  .row {
    grid-template-columns: 2.25rem 1fr 3.5rem;
  }

  .row__type,
  .row__stats,
  .row__set {
    display: none;
  }
}
</style>
