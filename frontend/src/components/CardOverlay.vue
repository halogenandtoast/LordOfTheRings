<script setup lang="ts">
import { useCardOverlay } from '@/composables/useCardOverlay'

const { src, sideways, placement, visible } = useCardOverlay()
</script>

<template>
  <Teleport to="body">
    <img
      v-if="visible && src"
      class="card-overlay"
      :class="{ sideways }"
      :src="src"
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
  box-shadow: 0 10px 40px rgb(0 0 0 / 0.85);
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
