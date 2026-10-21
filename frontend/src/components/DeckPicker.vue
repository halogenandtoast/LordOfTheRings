<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import DeckLoader from './DeckLoader.vue'
import DeckTile from './DeckTile.vue'
import SegmentedToggle from './SegmentedToggle.vue'
import { deckFaces, type DeckFace, type DeckKey } from '../deckChoice'
import type { Deck, DeckList, DeckSummary } from '../browser/types/deck'

// `loaded` is a deck loaded here for this game alone, chosen by the key 'loaded'
const { decks, loaded = null } = defineProps<{ decks: DeckSummary[]; loaded?: DeckFace | null }>()
const model = defineModel<DeckKey>({ required: true })
const emit = defineEmits<{ load: [face: DeckFace, list: DeckList]; saved: [deck: Deck] }>()
// open while a deck is being loaded, so the lobby can hold back starting
const loading = defineModel<boolean>('loading', { default: false })
const { t } = useI18n()

// your own decks once you have some, the starters until then
type Tab = 'mine' | 'starter'
const chosenTab = ref<Tab | null>(null)
const tab = computed<Tab>(() => chosenTab.value ?? (decks.length || loaded ? 'mine' : 'starter'))
const tabs = computed(() => [
  { value: 'mine' as const, label: t('newGame.myDecks'), count: decks.length },
  { value: 'starter' as const, label: t('newGame.starterTab') },
])
const faces = computed(() => [...(loaded ? [loaded] : []), ...deckFaces(decks)].filter((f) => (tab.value === 'starter') === (f.sphere !== null)))

const use = (face: DeckFace, list: DeckList) => {
  emit('load', face, list)
  model.value = face.key
  chosenTab.value = 'mine'
  loading.value = false
}
const saved = (deck: Deck) => {
  emit('saved', deck)
  model.value = `deck:${deck.id}`
  chosenTab.value = 'mine'
  loading.value = false
}

// the tab shown holds the deck chosen, so your first deck once they arrive
watch(
  () => decks.length,
  () => {
    if (!chosenTab.value && decks.length && model.value.startsWith('starter:')) model.value = `deck:${decks[0].id}`
  },
  { immediate: true },
)
</script>

<template>
  <DeckLoader v-if="loading" @use="use" @saved="saved" @cancel="loading = false" />
  <template v-else>
    <SegmentedToggle :options="tabs" :label="$t('newGame.deck')" :model-value="tab" @update:model-value="chosenTab = $event" />
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
      <button v-if="tab === 'mine'" type="button" class="import" @click="loading = true">{{ $t('newGame.importDeck') }}</button>
    </div>
  </template>
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
  background: none;
  color: var(--text-muted);
  font: inherit;
  font-size: 0.85rem;
  cursor: pointer;
}
.import:hover { border-color: var(--line-strong); color: var(--text); }
</style>
