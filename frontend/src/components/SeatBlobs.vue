<script setup lang="ts">
import { computed, ref } from 'vue'
import type { PlayerView } from '../types'

// A zone as a blob per seat: yours first, then the others to its right, each a preview of
// that seat. The blob pointed at grows to the big share and the rest share what is left.
// Pointing is followed across the whole row, so passing over a gap keeps the last blob big.
const props = defineProps<{ players: PlayerView[]; viewing: string | null }>()

// the big share by table size; the others split the rest evenly
const bigShare: Record<number, number> = { 1: 100, 2: 75, 3: 64, 4: 52 }

const seats = computed(() => {
  const mine = props.players.find((player) => player.playerId === props.viewing) ?? props.players[0]
  return mine ? [mine, ...props.players.filter((player) => player !== mine)] : []
})
const pointed = ref<string | null>(null)
const big = computed(() => pointed.value ?? seats.value[0]?.playerId)
const share = (playerId: string) => {
  const n = seats.value.length
  const largest = bigShare[n] ?? 100 / n
  return playerId === big.value ? largest : (100 - largest) / (n - 1)
}
</script>

<template>
  <div class="seat-blobs" @pointerleave="pointed = null" @focusout="pointed = null">
    <div
      v-for="(player, i) in seats"
      :key="player.playerId"
      class="blob"
      :style="{ flexGrow: share(player.playerId) }"
      @pointerenter="pointed = player.playerId"
      @focusin="pointed = player.playerId"
    >
      <span v-if="i > 0" class="name">{{ player.name }}</span>
      <slot :player="player" />
    </div>
  </div>
</template>

<style scoped>
.seat-blobs {
  display: flex;
  gap: var(--gap);
}
.blob {
  position: relative;
  display: flex;
  flex: 0 1 0;
  min-width: 0;
  overflow: hidden;
  border-radius: 6px;
  background: var(--zone-base);
  /* eases in, no overshoot */
  transition: flex-grow 0.32s cubic-bezier(0.4, 0.1, 0.2, 1);
}
.name {
  position: absolute;
  top: var(--gap);
  right: 6px;
  z-index: 1;
  font-size: 0.7rem;
  font-weight: 600;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  white-space: nowrap;
  color: rgb(from var(--text) r g b / 0.7);
  pointer-events: none;
}
@media (prefers-reduced-motion: reduce) { .blob { transition: none; } }
</style>
