<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import DeckTile from './DeckTile.vue'
import SegmentedToggle from './SegmentedToggle.vue'
import { deckFaces, type DeckKey } from '../deckChoice'
import type { DeckSummary } from '../browser/types/deck'

const { decks } = defineProps<{ decks: DeckSummary[] }>()
const model = defineModel<DeckKey>({ required: true })
const { t } = useI18n()

// your own decks once you have some, the starters until then
type Tab = 'mine' | 'starter'
const chosenTab = ref<Tab | null>(null)
const tab = computed<Tab>(() => chosenTab.value ?? (decks.length ? 'mine' : 'starter'))
const tabs = computed(() => [
  { value: 'mine' as const, label: t('newGame.myDecks'), count: decks.length },
  { value: 'starter' as const, label: t('newGame.starterTab') },
])
const faces = computed(() => deckFaces(decks).filter((f) => (tab.value === 'starter') === (f.sphere !== null)))

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
    <RouterLink v-if="tab === 'mine'" to="/decks" class="import">{{ $t('newGame.importDeck') }}</RouterLink>
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
