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
/* a notch cut out of the zone's top corner, the page showing through; the shown seat's
   tab is the zone itself rising into it, so it reads as a folder tab */
.zone-seats {
  --cut: 6px;
  position: absolute;
  top: -1px;
  right: -1px;
  z-index: 2;
  display: flex;
  align-items: flex-end;
  gap: 2px;
  padding: 3px 3px 0;
  background: var(--page-background);
  border-radius: 0 6px 0 var(--cut);
}
/* the zone's edges round off into the cut */
.zone-seats::before, .zone-seats::after {
  content: '';
  position: absolute;
  width: var(--cut);
  height: var(--cut);
  background: radial-gradient(circle at 0 100%, transparent var(--cut), var(--page-background) calc(var(--cut) + 0.5px));
}
.zone-seats::before { top: 0; left: calc(-1 * var(--cut)); }
.zone-seats::after { right: 0; bottom: calc(-1 * var(--cut)); }
button {
  padding: 2px 8px;
  margin-top: 3px;
  border: 0;
  border-radius: 5px 5px 0 0;
  background: none;
  color: var(--text-muted);
  font: inherit;
  font-size: 0.65rem;
  cursor: pointer;
  transition: margin-top 0.15s ease, background-color 0.15s ease, color 0.15s ease;
}
button:hover { color: var(--text); margin-top: 1px; }
button.active {
  margin-top: 0;
  padding-bottom: 4px;
  background: linear-gradient(var(--zone-bg), var(--zone-bg)) var(--page-background);
  color: var(--text);
  cursor: default;
}
@media (prefers-reduced-motion: reduce) { button { transition: none; } }
</style>
