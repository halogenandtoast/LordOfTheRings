<script lang="ts">
import type { PlayerView } from '../types'

/** How far round the hue wheel a seat's colour is turned from the zone's, so each seat has
    its own shade of it: 0, then 18° either side, then 36°. */
export const seatHue = (players: PlayerView[], playerId: string | null | undefined) => {
  const seat = Math.max(0, players.findIndex((player) => player.playerId === playerId))
  return Math.ceil(seat / 2) * 18 * (seat % 2 ? 1 : -1)
}
</script>

<script setup lang="ts">
import { computed, nextTick, ref, useTemplateRef, watch } from 'vue'
import { useResizeObserver } from '@vueuse/core'
import { Columns2, FoldHorizontal } from 'lucide-vue-next'

// A segmented switch over a zone's corner, for showing another seat's cards in just that
// zone, floating in its top corner: a seat per segment, its first hero's face beside its name, and a thumb in the
// seat's shade that glides under the one shown. Its last segment splits the zone between
// every seat, and then each seat's segment floats over its own share.
const props = defineProps<{ players: PlayerView[]; viewing: string | null; split: boolean }>()
const emit = defineEmits<{ view: [playerId: string]; split: [] }>()

const face = (player: PlayerView) => player.inPlay.find((card) => card.type === 'Hero')?.code

const segments: HTMLButtonElement[] = []
const thumb = ref<{ left: number; width: number } | null>(null)
const shown = computed(() =>
  props.split ? -1 : props.players.findIndex((player) => player.playerId === props.viewing),
)
const place = async () => {
  await nextTick()
  const segment = segments[shown.value]
  thumb.value = segment ? { left: segment.offsetLeft, width: segment.offsetWidth } : null
}
// again whenever the switch changes size, as when its font comes in
useResizeObserver(useTemplateRef('nav'), () => void place())
watch([shown, () => props.players.length], place)
</script>

<template>
  <nav ref="nav" class="zone-seats" :class="{ split }" role="tablist" :aria-label="$t('game.seats.label')">
    <span
      v-if="thumb"
      class="thumb"
      :style="{
        '--seat-hue': seatHue(players, viewing),
        translate: `${thumb.left}px 0`,
        width: `${thumb.width}px`,
      }"
    />
    <button
      v-for="(player, i) in players"
      :key="player.playerId"
      :ref="(el) => (segments[i] = el as HTMLButtonElement)"
      type="button"
      role="tab"
      :class="{ active: split || player.playerId === viewing }"
      :aria-selected="!split && player.playerId === viewing"
      @click="emit('view', player.playerId)"
    >
      <span
        class="avatar"
        :style="{
          '--seat-hue': seatHue(players, player.playerId),
          backgroundImage: face(player) ? `url(/images/cards/${face(player)}.avif)` : undefined,
        }"
      />
      {{ player.name }}
    </button>
    <button
      type="button"
      class="split-toggle"
      :class="{ active: split }"
      :aria-pressed="split"
      :title="$t(split ? 'game.seats.collapse' : 'game.seats.split')"
      :aria-label="$t(split ? 'game.seats.collapse' : 'game.seats.split')"
      @click="emit('split')"
    >
      <FoldHorizontal v-if="split" :size="16" />
      <Columns2 v-else :size="16" />
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
  gap: 6px;
  padding: 3px;
  border: 1px solid rgb(255 255 255 / 0.08);
  border-radius: 999px;
  background: rgb(0 0 0 / 0.35);
  backdrop-filter: blur(8px);
  box-shadow: 0 2px 8px rgb(0 0 0 / 0.3);
}
/* the zone's colour turned to a seat's hue (see seatHue), lifted to read on the dark */
.thumb, .avatar { --seat: oklch(from var(--zone-base) 0.62 calc(c * 0.9) calc(h + var(--seat-hue)) / 1); }
.thumb {
  position: absolute;
  top: 3px;
  bottom: 3px;
  left: 0;
  border-radius: 999px;
  background: rgb(from var(--seat) r g b / 0.32);
  box-shadow: inset 0 0 0 1px rgb(from var(--seat) r g b / 0.5);
  transition: translate 0.22s cubic-bezier(0.3, 0.7, 0.4, 1), width 0.22s cubic-bezier(0.3, 0.7, 0.4, 1);
}
button {
  position: relative;
  display: inline-flex;
  align-items: center;
  gap: 7px;
  height: 28px;
  margin: 0;
  padding: 0 12px 0 3px;
  border: 0;
  border-radius: 999px;
  background: none;
  color: rgb(from var(--text) r g b / 0.6);
  font: inherit;
  font-size: 0.8rem;
  font-weight: 600;
  white-space: nowrap;
  cursor: pointer;
  transition: color 0.15s;
}
button:hover, button.active { color: var(--text); }
button.active { cursor: default; }
button:focus-visible { outline: 1px solid var(--text); outline-offset: 1px; }
/* the face in the hero's art, high on the card and right of the stats */
.avatar {
  flex: none;
  width: 22px;
  height: 22px;
  border-radius: 50%;
  background: 64% 4% / 300% auto no-repeat var(--seat);
  box-shadow: 0 0 0 1.5px var(--seat);
  filter: saturate(0.7);
  transition: filter 0.15s;
}
button:hover .avatar, button.active .avatar { filter: none; }
.split-toggle { padding: 0 6px; }
.split-toggle.active { background: rgb(255 255 255 / 0.12); cursor: pointer; }
/* split: a column per seat across the zone (whose shares are even, see Game.vue), each
   seat's segment a pill of its own centred over its share, the toggle at the far end */
.zone-seats.split {
  left: 0;
  display: grid;
  grid-auto-flow: column;
  grid-auto-columns: 1fr;
  justify-items: center;
  padding: 0;
  border: 0;
  background: none;
  backdrop-filter: none;
  box-shadow: none;
  pointer-events: none;
}
.zone-seats.split > button {
  padding: 3px 15px 3px 6px;
  height: 34px;
  border: 1px solid rgb(255 255 255 / 0.08);
  background: rgb(0 0 0 / 0.35);
  backdrop-filter: blur(8px);
  box-shadow: 0 2px 8px rgb(0 0 0 / 0.3);
  pointer-events: auto;
  cursor: pointer;
}
.zone-seats.split > .split-toggle { position: absolute; top: 0; right: 0; padding: 0 9px; background: rgb(255 255 255 / 0.12); }
@media (prefers-reduced-motion: reduce) { .thumb { transition: none; } }
</style>
