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
/* a row of folder tabs of its own, along the top of the zone (which leaves room for it,
   see Game.vue); the shown seat's tab is in the zone's colour, so it joins the zone */
.zone-seats {
  position: absolute;
  bottom: 100%;
  right: 0;
  display: flex;
  align-items: flex-end;
}
button {
  /* the zone's colour turned to this seat's hue (see seatHue), stronger while it waits */
  --seat-zone: oklch(from var(--zone-base) l c calc(h + var(--seat-hue)));
  /* straight on the left, the top right corner cut off in a slope */
  --slope: 8px;
  padding: 2px calc(6px + var(--slope)) 2px 6px;
  margin-top: 3px;
  border: 0;
  clip-path: polygon(0 0, calc(100% - var(--slope)) 0, 100% 100%, 0 100%);
  background: oklch(from var(--zone-base) l c calc(h + var(--seat-hue)) / calc(alpha * 2.5));
  color: var(--text-muted);
  font: inherit;
  font-size: 0.65rem;
  line-height: 13px;
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
