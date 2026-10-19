<script setup lang="ts">
import { computed } from 'vue'
import { useCardOverlay } from '../composables/useCardOverlay'
import { cardImage } from '../types'

const { card, sideways, placement, visible } = useCardOverlay()

const source = computed(() => (card.value ? cardImage(card.value) : ''))
</script>

<template>
  <Teleport to="body">
    <img
      v-if="visible && card"
      class="card-overlay"
      :class="{ sideways }"
      :src="source"
      :alt="card.name"
      :style="{
        top: `${placement.top}px`,
        left: `${placement.left}px`,
        width: `${placement.width}px`,
      }"
    />
  </Teleport>
</template>

<style scoped>
.card-overlay {
  position: fixed;
  z-index: 9999;
  border-radius: 12px;
  box-shadow: 0 10px 40px var(--shadow-strong);
  pointer-events: none;
  animation: overlay-in 0.12s ease-out;
}

@keyframes overlay-in {
  from { opacity: 0; }
  to { opacity: 1; }
}

@media (prefers-reduced-motion: reduce) {
  .card-overlay { animation: none; }
}
</style>
