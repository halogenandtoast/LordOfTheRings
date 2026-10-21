<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import DeckImport from '../browser/components/DeckImport.vue'
import DeckLoader from './DeckLoader.vue'
import DeckTile from './DeckTile.vue'
import SegmentedToggle from './SegmentedToggle.vue'
import { deckFaces, type DeckFace, type DeckKey } from '../deckChoice'
import type { Deck, DeckList, DeckPreview, DeckSummary, ImportRequest } from '../browser/types/deck'
import '../browser/browser.css'

// `loaded` is a deck loaded here for this game alone, chosen by the key 'loaded'
const { decks, loaded = null } = defineProps<{ decks: DeckSummary[]; loaded?: DeckFace | null }>()
const model = defineModel<DeckKey>({ required: true })
const emit = defineEmits<{ load: [face: DeckFace, list: DeckList]; saved: [deck: Deck] }>()
// open while a loaded deck is looked over, so the lobby can hold back starting
const loading = defineModel<boolean>('loading', { default: false })
const pending = ref<{ preview: DeckPreview; request: ImportRequest } | null>(null)
const look = (preview: DeckPreview, request: ImportRequest) => {
  pending.value = { preview, request }
  loading.value = true
}
const close = () => {
  pending.value = null
  loading.value = false
}
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
  close()
}
const saved = (deck: Deck) => {
  emit('saved', deck)
  model.value = `deck:${deck.id}`
  chosenTab.value = 'mine'
  close()
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
  <DeckLoader v-if="pending" :loaded="pending" @use="use" @saved="saved" @cancel="close" />
  <template v-else>
    <SegmentedToggle :options="tabs" :label="$t('newGame.deck')" :model-value="tab" @update:model-value="chosenTab = $event" />
    <!-- the browser's tokens for the row, as on the decks page, not its page background -->
    <div v-if="tab === 'mine'" class="card-browser bare">
      <DeckImport :keep="false" @loaded="look" />
    </div>
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
.bare { background: none; }
</style>
