<script setup lang="ts">
/**
 * One card in the grid view: the scan, with its name underneath.
 */
import CardImage from './CardImage.vue'
import type { Card } from '@browser/types/card'

const { card, selected = false } = defineProps<{ card: Card; selected?: boolean }>()
</script>

<template>
  <article
    class="tile"
    :class="{ 'tile--selected': selected }"
    :data-sphere="card.sphere ?? undefined"
  >
    <CardImage :card="card" size="thumb" />
    <div class="tile__caption">
      <span class="tile__name truncate">
        <span v-if="card.unique" class="unique" aria-label="Unique">&#9670;</span>
        {{ card.name }}
      </span>
      <span class="tile__set truncate">{{ card.packName }}</span>
    </div>
  </article>
</template>

<style scoped>
.tile {
  display: flex;
  flex-direction: column;
  gap: var(--space-2);
  border-radius: var(--radius);
  transition:
    transform var(--transition),
    filter var(--transition);
}

.tile:hover {
  transform: translateY(-2px);
  filter: brightness(1.08);
}

.tile--selected :deep(.frame) {
  outline: 2px solid var(--c-accent);
  outline-offset: 2px;
}

.tile__caption {
  display: flex;
  flex-direction: column;
  min-width: 0;
  line-height: 1.3;
}

.tile__name {
  font-size: var(--text-sm);
}

.unique {
  color: var(--sphere-color, var(--c-accent));
  font-size: 0.7em;
}

.tile__set {
  font-size: var(--text-xs);
  color: var(--c-text-faint);
}
</style>
