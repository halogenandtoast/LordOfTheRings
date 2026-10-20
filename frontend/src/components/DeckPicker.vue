<script setup lang="ts">
import { computed } from 'vue'
import DeckTile from './DeckTile.vue'
import { deckFaces, type DeckKey } from '../deckChoice'
import type { DeckSummary } from '../browser/types/deck'

const { decks } = defineProps<{ decks: DeckSummary[] }>()
const model = defineModel<DeckKey>({ required: true })
const faces = computed(() => deckFaces(decks))
</script>

<template>
  <div class="deck-picker" role="radiogroup">
    <button
      v-for="face in faces"
      :key="face.key"
      type="button"
      role="radio"
      :aria-checked="model === face.key"
      class="pick"
      @click="model = face.key"
    >
      <DeckTile :face="face" :selected="model === face.key" />
    </button>
    <RouterLink to="/decks" class="import">{{ $t('newGame.importDeck') }}</RouterLink>
  </div>
</template>

<style scoped>
.deck-picker {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
  gap: 10px;
}
.pick { display: grid; padding: 0; border: 0; background: none; font: inherit; cursor: pointer; }
.pick:hover :deep(.deck-tile:not(.selected)) { border-color: var(--line-strong); transform: translateY(-2px); }
.import {
  display: grid;
  place-items: center;
  min-height: 90px;
  border: 2px dashed var(--line);
  border-radius: 6px;
  color: var(--text-muted);
  font-size: 0.85rem;
  text-decoration: none;
}
.import:hover { border-color: var(--line-strong); color: var(--text); }
</style>
