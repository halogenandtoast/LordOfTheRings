<script setup lang="ts">
/**
 * One counted list of cards — the heroes, a card type within the draw deck, or
 * the sideboard.
 *
 * A slot whose card did not resolve still gets a line, showing the code the
 * source used: better a visible gap than a deck that is quietly three cards
 * short.
 */
import { computed } from 'vue'
import GameIcon from './GameIcon.vue'
import type { Slot } from '@browser/types/deck'
import type { IconName } from '@browser/lib/card-text'
import { primaryCost } from '@browser/types/card'

const { title, slots } = defineProps<{ title: string; slots: Slot[] }>()

const total = computed(() => slots.reduce((n, slot) => n + slot.quantity, 0))

/** Only the spheres with a printed glyph; the rest fall back to a colour dot. */
const GLYPHS = ['leadership', 'lore', 'spirit', 'tactics', 'baggins', 'fellowship', 'neutral']

function sphereIcon(slot: Slot): IconName | null {
  const sphere = slot.card?.sphere
  return sphere && GLYPHS.includes(sphere) ? (sphere as IconName) : null
}

function cost(slot: Slot): number | 'X' | null {
  return slot.card ? primaryCost(slot.card) : null
}
</script>

<template>
  <section class="section">
    <h3 class="section__title">
      {{ title }}
      <span class="section__count">{{ total }}</span>
    </h3>

    <ul class="slots">
      <li
        v-for="slot in slots"
        :key="slot.sourceCode"
        class="slot"
        :data-sphere="slot.card?.sphere ?? undefined"
      >
        <span class="slot__quantity">{{ slot.quantity }}&#215;</span>

        <template v-if="slot.card">
          <RouterLink class="slot__name truncate" :to="`/cards/${slot.card.code}`">
            <span v-if="slot.card.unique" class="unique" aria-label="Unique">&#9670;</span>
            {{ slot.card.name }}
          </RouterLink>

          <span class="slot__cost">
            <template v-if="cost(slot) !== null">{{ cost(slot) }}</template>
          </span>

          <GameIcon v-if="sphereIcon(slot)" :name="sphereIcon(slot)!" class="slot__sphere" />
          <span v-else-if="slot.card.sphere" class="sphere-dot" :title="slot.card.sphere" />
          <span v-else class="slot__sphere" />
        </template>

        <template v-else>
          <span class="slot__name slot__name--unknown">
            Unknown card <code>{{ slot.sourceCode }}</code>
          </span>
          <span class="slot__cost" />
          <span class="slot__sphere" />
        </template>
      </li>
    </ul>
  </section>
</template>

<style scoped>
.section__title {
  display: flex;
  align-items: baseline;
  gap: var(--space-2);
  padding-bottom: var(--space-1);
  border-bottom: 1px solid var(--c-border);
  font-family: var(--font-display);
  font-size: var(--text-base);
}

.section__count {
  color: var(--c-text-faint);
  font-size: var(--text-xs);
  font-variant-numeric: tabular-nums;
}

.slots {
  margin-top: var(--space-2);
}

.slot {
  display: grid;
  grid-template-columns: 1.75rem minmax(0, 1fr) 1.25rem 1rem;
  align-items: center;
  gap: var(--space-2);
  padding: 0.125rem 0;
  font-size: var(--text-sm);
}

.slot__quantity {
  color: var(--c-text-faint);
  font-size: var(--text-xs);
  font-variant-numeric: tabular-nums;
  text-align: right;
}

.slot__name:hover {
  color: var(--c-accent);
}

.slot__name--unknown {
  color: var(--c-danger);
}

.unique {
  color: var(--c-accent);
  font-size: 0.7em;
  vertical-align: 0.15em;
}

.slot__cost {
  color: var(--c-text-muted);
  font-variant-numeric: tabular-nums;
  text-align: right;
}

.slot__sphere {
  display: inline-flex;
  justify-content: center;
  width: 1rem;
  color: var(--sphere-color, var(--c-text-faint));
}

.sphere-dot {
  justify-self: center;
  width: 0.625rem;
  height: 0.625rem;
  border-radius: 50%;
  background: var(--sphere-color, var(--c-text-faint));
}

code {
  font-family: var(--font-mono);
  font-size: 0.9em;
}
</style>
