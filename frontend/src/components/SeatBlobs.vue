<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import type { PlayerView } from '../types'

// A zone as a blob per seat: yours first, then the others to its right, each a preview of
// that seat. Clicking a small blob grows it to the big share, the rest sharing what is left;
// that first click only picks the blob, so it never answers with a card it can barely show.
const props = defineProps<{ players: PlayerView[]; viewing: string | null }>()

// the big share by table size; the others split the rest evenly
const bigShare: Record<number, number> = { 1: 100, 2: 75, 3: 64, 4: 52 }

const seats = computed(() => {
  const mine = props.players.find((player) => player.playerId === props.viewing) ?? props.players[0]
  return mine ? [mine, ...props.players.filter((player) => player !== mine)] : []
})
const picked = ref<string | null>(null)
watch(() => props.viewing, () => (picked.value = null))
const big = computed(() => picked.value ?? seats.value[0]?.playerId)
const pick = (event: MouseEvent, playerId: string) => {
  if (playerId === big.value) return
  event.stopPropagation()
  event.preventDefault()
  picked.value = playerId
}
const share = (playerId: string) => {
  const n = seats.value.length
  const largest = bigShare[n] ?? 100 / n
  return playerId === big.value ? largest : (100 - largest) / (n - 1)
}
</script>

<template>
  <div class="seat-blobs">
    <div
      v-for="(player, i) in seats"
      :key="player.playerId"
      class="blob"
      :class="{ small: player.playerId !== big }"
      :style="{ flexGrow: share(player.playerId) }"
      @click.capture="pick($event, player.playerId)"
    >
      <span v-if="i > 0" class="name">{{ player.name }}</span>
      <slot :player="player" :small="player.playerId !== big" />
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
.blob.small { cursor: pointer; }
.blob.small :deep(*) { cursor: inherit; }
.blob.small:hover { filter: brightness(1.15); }
@media (prefers-reduced-motion: reduce) { .blob { transition: none; } }
</style>
