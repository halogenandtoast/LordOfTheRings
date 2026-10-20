<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, ref, useTemplateRef } from 'vue'
import GameCard from './GameCard.vue'
import type { CardView } from '../types'

// the victory display is the same pile: an empty place for a card, the count opening the rest
const { cards, back, kind = 'discard' } = defineProps<{
  cards: CardView[]
  back: 'player' | 'encounter' | 'victory'
  kind?: 'discard' | 'victory'
}>()

const top = computed(() => cards.at(-1) ?? null)
// most recently discarded first
const pile = computed(() => [...cards].reverse())

// Like Arkham's discard: the count opens a panel of every card in the pile. It is a plain
// fixed panel rather than a top-layer popover so the card hover overlay still shows above it.
const open = ref(false)
const position = ref<Record<string, string>>({})
const button = useTemplateRef<HTMLElement>('button')
const panel = useTemplateRef<HTMLElement>('panel')

const place = () => {
  if (!button.value || !panel.value) return
  const anchor = button.value.getBoundingClientRect()
  const width = panel.value.offsetWidth
  const left = Math.min(Math.max(8, anchor.left + anchor.width / 2 - width / 2), innerWidth - width - 8)
  // open away from the screen edge the pile sits nearest
  position.value =
    anchor.top > innerHeight / 2
      ? { left: `${left}px`, bottom: `${innerHeight - anchor.top + 8}px` }
      : { left: `${left}px`, top: `${anchor.bottom + 8}px` }
}

const close = () => {
  open.value = false
  document.removeEventListener('pointerdown', onPointerDown)
  document.removeEventListener('keydown', onKeydown)
}

const onPointerDown = (event: PointerEvent) => {
  const target = event.target as Node
  if (!panel.value?.contains(target) && !button.value?.contains(target)) close()
}

const onKeydown = (event: KeyboardEvent) => {
  if (event.key === 'Escape') close()
}

const toggle = async () => {
  if (open.value) return close()
  open.value = true
  position.value = { visibility: 'hidden' }
  await nextTick()
  place()
  document.addEventListener('pointerdown', onPointerDown)
  document.addEventListener('keydown', onKeydown)
}

onBeforeUnmount(close)
</script>

<template>
  <div class="pile-stack">
    <div :class="kind">
      <div v-if="!top" class="pile" :class="[back, 'empty']" />
      <GameCard v-else :card="top" />
    </div>
    <button
      v-if="cards.length"
      ref="button"
      type="button"
      class="pile-count pile-button"
      :class="{ open }"
      :aria-expanded="open"
      :title="$t(`game.${kind}.title`, { count: cards.length })"
      @click="toggle"
    >
      <span class="stack-glyph" aria-hidden="true"><span /><span /></span>
      {{ cards.length }}
    </button>
    <span v-else class="pile-count">0</span>

    <Teleport to="body">
      <div v-if="open" ref="panel" class="discard-panel" :style="position">
        <div class="discard-panel-header">{{ $t(`game.${kind}.header`, { count: cards.length }) }}</div>
        <div class="discard-panel-cards">
          <GameCard v-for="card in pile" :key="card.id" :card="card" />
        </div>
      </div>
    </Teleport>
  </div>
</template>

<style scoped>
/* Dimmed, not see-through. The dimming goes on the card's image because each card is
   its own view transition snapshot, drawn without effects from wrappers like this one. */
.discard { display: grid; }
.discard :deep(img) { filter: brightness(0.55); transition: filter 0.15s ease; }
.pile-stack:hover .discard :deep(img) { filter: none; }

.pile-button {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  cursor: pointer;
  font: inherit;
  transition: background 0.15s ease, border-color 0.15s ease, transform 0.15s ease;
}
.pile-button:hover, .pile-button.open { border-color: var(--cream); transform: translateY(-1px); }

/* two little stacked cards, as on Arkham's pile buttons */
.stack-glyph { display: inline-flex; align-items: center; }
.stack-glyph span {
  width: 8px;
  height: 11px;
  border-radius: 2px;
  border: 1px solid currentColor;
  background: rgb(255 255 255 / 0.18);
}
.stack-glyph span:first-child { opacity: 0.55; transform: rotate(-6deg) translateY(1px); }
.stack-glyph span:last-child { margin-left: -4px; transform: rotate(5deg); }

.discard-panel {
  position: fixed;
  z-index: 9000; /* under the card overlay */
  max-width: min(94vw, 640px);
  padding: 10px;
  border-radius: 10px;
  border: 1px solid rgb(255 255 255 / 0.12);
  background: rgb(20 16 10 / 0.94);
  backdrop-filter: blur(8px);
  box-shadow: 0 8px 30px rgb(0 0 0 / 0.5);
  color: var(--cream);
}
.discard-panel-header {
  margin: 0 0 8px;
  font-size: 0.75rem;
  font-weight: 700;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  opacity: 0.82;
}
.discard-panel-cards {
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
  max-height: 50vh;
  overflow: auto;
}
.discard-panel-cards :deep(img) { filter: grayscale(0.85); transition: filter 0.15s ease; }
.discard-panel-cards :deep(.card:hover img) { filter: none; }
</style>
