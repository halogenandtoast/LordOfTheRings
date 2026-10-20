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
  /* the zone's colour turned to this seat's hue (see seatHue), as solid shades */
  --seat-dim: oklch(from var(--zone-base) 0.3 calc(c * 0.7) calc(h + var(--seat-hue)) / 1);
  --seat-hover: oklch(from var(--zone-base) 0.36 calc(c * 0.8) calc(h + var(--seat-hue)) / 1);
  --seat-bright: oklch(from var(--zone-base) 0.78 c calc(h + var(--seat-hue)) / 1);
  /* straight on the left, the top right corner cut off in a slope */
  --slope: 10px;
  padding: 4px calc(10px + var(--slope)) 4px 10px;
  margin-top: 3px;
  border: 0;
  clip-path: polygon(0 0, calc(100% - var(--slope)) 0, 100% 100%, 0 100%);
  background: var(--seat-dim);
  color: rgb(from var(--text) r g b / 0.75);
  font: inherit;
  font-size: 0.75rem;
  font-weight: 600;
  line-height: 14px;
  white-space: nowrap;
  cursor: pointer;
  transition: margin-top 0.15s ease, background 0.15s ease, color 0.15s ease;
}
button:hover { color: var(--text); background: var(--seat-hover); margin-top: 1px; }
button:focus-visible { outline: 2px solid var(--seat-bright); outline-offset: -2px; }
/* the zone's own colour, so the tab joins the zone, with a stripe of the seat's shade */
button.active {
  margin-top: 0;
  padding-bottom: 7px;
  background: linear-gradient(var(--zone-base), var(--zone-base)) var(--page-background);
  box-shadow: inset 0 3px 0 var(--seat-bright);
  color: var(--text);
  font-weight: 700;
  cursor: default;
}
@media (prefers-reduced-motion: reduce) { button { transition: none; } }
</style>
