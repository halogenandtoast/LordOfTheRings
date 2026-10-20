<script lang="ts">
import type { PlayerView } from '../types'

/** How far round the hue wheel a seat's zones are turned, so each seat's tab has its own
    shade of the zone's colour: 0, then 18° either side, then 36°. */
export const seatHue = (players: PlayerView[], playerId: string | null | undefined) => {
  const seat = Math.max(0, players.findIndex((player) => player.playerId === playerId))
  return Math.ceil(seat / 2) * 18 * (seat % 2 ? 1 : -1)
}
</script>

<script setup lang="ts">

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
      :style="{ '--seat-hue': seatHue(players, player.playerId) }"
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
  /* the zone's colour turned to this seat's hue (see seatHue), stronger while it waits */
  --seat-zone: oklch(from var(--zone-base) l c calc(h + var(--seat-hue)));
  padding: 2px 8px;
  margin-top: 3px;
  border: 0;
  border-radius: 5px 5px 0 0;
  background: oklch(from var(--zone-base) l c calc(h + var(--seat-hue)) / calc(alpha * 2.5));
  color: var(--text-muted);
  font: inherit;
  font-size: 0.65rem;
  cursor: pointer;
  transition: margin-top 0.15s ease, background 0.15s ease, color 0.15s ease;
}
button:hover { color: var(--text); margin-top: 1px; }
button.active {
  margin-top: 0;
  padding-bottom: 4px;
  /* this seat's shade at the zone's strength, so the tab joins the zone */
  background: linear-gradient(var(--seat-zone), var(--seat-zone)) var(--page-background);
  color: var(--text);
  cursor: default;
}
@media (prefers-reduced-motion: reduce) { button { transition: none; } }
</style>
