<script setup lang="ts">
import { computed, nextTick, ref, useId, watch } from 'vue'
import type { PlayerView } from '../types'

// A zone as a blob per seat: yours first, then the others to its right. One seat is open
// and takes the row; the rest are shrunk to a narrow column. Clicking a shrunk blob opens
// it, and that first click only opens it, so it never answers with a card it can't show.
const props = defineProps<{ players: PlayerView[]; viewing: string | null }>()

const seats = computed(() => {
  const mine = props.players.find((player) => player.playerId === props.viewing) ?? props.players[0]
  return mine ? [mine, ...props.players.filter((player) => player !== mine)] : []
})
const picked = ref<string | null>(null)
watch(() => props.viewing, () => (picked.value = null))
const big = computed(() => picked.value ?? seats.value[0]?.playerId)

// Each blob morphs from where it was to where it lands, its contents fading across
// (see the view transition styles below), where the browser can.
const zone = useId()
const pick = (event: MouseEvent, playerId: string) => {
  if (playerId === big.value) return
  event.stopPropagation()
  event.preventDefault()
  const open = async () => {
    picked.value = playerId
    await nextTick()
  }
  if (!document.startViewTransition || matchMedia('(prefers-reduced-motion: reduce)').matches) void open()
  else document.startViewTransition(open)
}
</script>

<template>
  <div class="seat-blobs">
    <div
      v-for="(player, i) in seats"
      :key="player.playerId"
      class="blob"
      :class="{ small: player.playerId !== big }"
      :style="{ viewTransitionName: `seat-${zone}-${i}` }"
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
  flex: 1 1 0;
  min-width: 0;
  overflow: hidden;
  border-radius: 6px;
  background: var(--zone-base);
  view-transition-class: seat-blob;
}
/* as wide as what it shows, which is a narrow column */
.blob.small { flex: 0 0 auto; cursor: pointer; }
.blob.small :deep(*) { cursor: inherit; }
.blob.small:hover { filter: brightness(1.15); }
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
/* the blob's box moves and resizes; its contents are cropped to it, never stretched, the
   old fading out as the new fades in */
:global(::view-transition-group(.seat-blob)) {
  overflow: clip;
  border-radius: 6px;
  animation-duration: 0.36s;
  animation-timing-function: cubic-bezier(0.4, 0.1, 0.2, 1);
}
:global(::view-transition-old(.seat-blob)),
:global(::view-transition-new(.seat-blob)) {
  width: 100%;
  height: 100%;
  object-fit: none;
  object-position: left top;
  animation-duration: 0.36s;
}
</style>
