<script lang="ts">
import type { PlayerView } from '../types'

/** How far round the hue wheel a seat's handle is turned, so each seat has its own shade
    of the zone's colour: 0, then 18° either side, then 36°. */
export const seatHue = (players: PlayerView[], playerId: string | null | undefined) => {
  const seat = Math.max(0, players.findIndex((player) => player.playerId === playerId))
  return Math.ceil(seat / 2) * 18 * (seat % 2 ? 1 : -1)
}
</script>

<script setup lang="ts">
import { computed } from 'vue'

// A zone as a drawer per seat, as 3e's mythos panel does: each drawer has a handle down
// its left edge. Up to the seat shown they lie open, left to right, each covering the one
// before but its handle; the rest are shut against the right edge with only their handles
// showing. Taking a handle slides its drawer across.
const props = defineProps<{ players: PlayerView[]; viewing: string | null }>()
const emit = defineEmits<{ view: [playerId: string] }>()

const shown = computed(() => Math.max(0, props.players.findIndex((player) => player.playerId === props.viewing)))
</script>

<template>
  <div class="seat-drawers" :style="{ '--count': players.length }">
    <div
      v-for="(player, i) in players"
      :key="player.playerId"
      class="drawer"
      :class="{ open: i <= shown }"
      :style="{ '--i': i, '--seat-hue': seatHue(players, player.playerId), zIndex: i }"
    >
      <button
        v-if="players.length > 1"
        type="button"
        class="handle"
        :class="{ on: i === shown }"
        :aria-expanded="i === shown"
        :title="player.name"
        @click="emit('view', player.playerId)"
      >
        {{ player.name }}
      </button>
      <div class="contents" :inert="i !== shown">
        <slot :player="player" />
      </div>
    </div>
  </div>
</template>

<style scoped>
.seat-drawers {
  --handle: 22px;
  display: grid;
  overflow: hidden;
}
/* every drawer stops short of the far edge by the other handles, so the open one ends
   where the next one's handle begins */
.drawer {
  grid-area: 1 / 1;
  display: flex;
  min-width: 0;
  width: calc(100% - (var(--count) - 1) * var(--handle));
  margin-left: calc(var(--i) * var(--handle));
  /* opaque, so the drawer in front hides the one behind */
  background: linear-gradient(var(--zone-base), var(--zone-base)) var(--page-background);
  transition: margin-left 0.32s cubic-bezier(0.4, 0.1, 0.2, 1);
}
.drawer:not(.open) { margin-left: calc(100% - (var(--count) - var(--i)) * var(--handle)); }
.handle {
  --seat: oklch(from var(--zone-base) 0.5 calc(c * 0.8) calc(h + var(--seat-hue)));
  flex: 0 0 var(--handle);
  width: var(--handle);
  margin: 0;
  padding: 8px 0;
  border: 0;
  /* turned upside down, so this rule is on its left */
  border-right: 1px solid rgb(from var(--seat) r g b / 0.45);
  background: rgb(from var(--seat) r g b / 0.18);
  color: rgb(from var(--text) r g b / 0.7);
  font: inherit;
  font-size: 0.7rem;
  font-weight: 600;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  writing-mode: vertical-rl;
  rotate: 180deg;
  cursor: pointer;
  transition: background 0.14s, color 0.14s;
}
.handle:hover { color: var(--text); background: rgb(from var(--seat) r g b / 0.3); }
.handle.on { color: var(--text); background: rgb(from var(--seat) r g b / 0.4); cursor: default; }
.handle:focus-visible { outline: 1px solid var(--text); outline-offset: -3px; }
.contents { flex: 1 1 auto; min-width: 0; display: flex; }
@media (prefers-reduced-motion: reduce) { .drawer { transition: none; } }
</style>
