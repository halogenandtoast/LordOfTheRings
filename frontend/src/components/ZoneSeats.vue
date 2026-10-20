<script setup lang="ts">
import type { PlayerView } from '../types'

// Small tabs in a zone's corner, for showing another seat's cards in just that zone.
defineProps<{ players: PlayerView[]; viewing: string | null }>()
const emit = defineEmits<{ view: [playerId: string] }>()
</script>

<template>
  <nav class="zone-seats" role="tablist" :aria-label="$t('game.seats.label')">
    <button
      v-for="player in players"
      :key="player.playerId"
      type="button"
      role="tab"
      :class="{ active: player.playerId === viewing }"
      :aria-selected="player.playerId === viewing"
      @click="emit('view', player.playerId)"
    >
      {{ player.name }}
    </button>
  </nav>
</template>

<style scoped>
.zone-seats {
  position: absolute;
  top: 4px;
  right: 4px;
  z-index: 2;
  display: flex;
  gap: 2px;
}
button {
  padding: 1px 6px;
  border: 1px solid var(--line);
  border-radius: 4px;
  background: var(--surface-1);
  color: var(--text-muted);
  font: inherit;
  font-size: 0.65rem;
  cursor: pointer;
}
button:hover { color: var(--text); }
button.active { border-color: var(--line-strong); background: var(--surface-2); color: var(--text); }
</style>
