<script setup lang="ts">
import { computed, ref } from 'vue'
import DeckTile from './DeckTile.vue'
import { importDeck } from '../browser/lib/api'
import type { Deck, DeckList, DeckPreview, ImportRequest } from '../browser/types/deck'
import type { DeckFace } from '../deckChoice'
import { useServerText } from '../i18n'

// A deck loaded from the lobby, looked over before it is played: kept in your decks
// only when asked, so a deck to try needs no trip to the decks page.
const { loaded } = defineProps<{ loaded: { preview: DeckPreview; request: ImportRequest } }>()
const emit = defineEmits<{ use: [face: DeckFace, list: DeckList]; saved: [deck: Deck]; cancel: [] }>()
const tr = useServerText()

const save = ref(false)
const busy = ref(false)
const error = ref<string | null>(null)

const face = computed((): DeckFace => {
  const { preview } = loaded
  const names = preview.deck.heroes.map((s) => s.card?.name ?? s.sourceCode)
  return { key: 'loaded', sphere: null, starter: null, name: preview.deck.name, heroCodes: preview.heroCodes, detail: names.join(', ') }
})

const use = async () => {
  const { preview, request } = loaded
  if (!save.value) return emit('use', face.value, preview.list!)
  busy.value = true
  error.value = null
  try {
    emit('saved', await importDeck({ ...request, save: true }))
  } catch (err) {
    error.value = err instanceof Error ? err.message : String(err)
  } finally {
    busy.value = false
  }
}
</script>

<template>
  <div class="deck-loader">
    <template v-if="face">
      <div class="preview">
        <DeckTile :face="face" />
        <dl class="details">
          <div><dt>{{ $t('newGame.loader.cards') }}</dt><dd>{{ loaded.preview.cardCount }}</dd></div>
          <div v-if="loaded.preview.deck.startingThreat !== null"><dt>{{ $t('newGame.loader.threat') }}</dt><dd>{{ loaded.preview.deck.startingThreat }}</dd></div>
        </dl>
      </div>
      <p v-if="loaded.preview.problem" class="problem" role="alert">{{ tr(loaded.preview.problem) }}</p>
      <label class="switch">
        <input v-model="save" type="checkbox" role="switch" />
        <span class="track" aria-hidden="true" />
        {{ $t('newGame.loader.save') }}
      </label>
      <p v-if="error" class="problem" role="alert">{{ tr(error) }}</p>
    </template>
    <div class="actions">
      <button type="button" class="secondary" @click="emit('cancel')">{{ $t('common.back') }}</button>
      <button type="button" class="primary" :disabled="busy || !loaded.preview.list" @click="use">{{ $t('newGame.loader.use') }}</button>
    </div>
  </div>
</template>

<style scoped>
.deck-loader { display: grid; gap: 12px; }
.preview { display: grid; grid-template-columns: 200px 1fr; gap: 16px; align-items: start; }
.details { display: grid; gap: 6px; margin: 0; }
.details div { display: flex; gap: 8px; }
.details dt { color: var(--text-muted); }
.details dd { margin: 0; font-weight: 700; }
.problem { margin: 0; color: var(--threat-red); }
.actions { display: flex; gap: 10px; }
.actions button {
  flex: 1;
  padding: 12px;
  border: 0;
  border-radius: 3px;
  font: inherit;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  cursor: pointer;
}
.primary { background: var(--button-primary); color: var(--button-primary-text); }
.primary:hover:not(:disabled) { background: var(--button-primary-hover); }
.primary:disabled { opacity: 0.5; cursor: not-allowed; }
.secondary { background: var(--button-secondary); color: var(--text); }
.secondary:hover { background: var(--button-secondary-hover); }
</style>
