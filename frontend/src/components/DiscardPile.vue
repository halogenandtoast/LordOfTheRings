<script setup lang="ts">
import { computed } from 'vue'
import GameCard from './GameCard.vue'
import type { CardView } from '../types'

const { cards, back } = defineProps<{ cards: CardView[]; back: 'player' | 'encounter' }>()

const top = computed(() => cards.at(-1) ?? null)
</script>

<template>
  <div class="discard">
    <div v-if="!top" class="pile" :class="[back, 'empty']">
      <span class="pile-count">0</span>
    </div>
    <template v-else>
      <GameCard :card="top" />
      <span class="pile-count">{{ cards.length }}</span>
    </template>
  </div>
</template>

<style scoped>
.discard { position: relative; display: grid; flex: none; opacity: 0.55; transition: opacity 0.15s ease; }
.discard:hover { opacity: 1; }
.discard > * { grid-area: 1 / 1; }
.pile-count {
  place-self: end center;
  position: relative;
  z-index: 3;
  margin-bottom: 4px;
  pointer-events: none;
}
</style>
