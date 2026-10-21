<script setup lang="ts">
import { computed, ref } from 'vue'
import DeckImport from '../browser/components/DeckImport.vue'
import DeckTile from './DeckTile.vue'
import { importDeck } from '../browser/lib/api'
import type { Deck, DeckList, DeckPreview, ImportRequest } from '../browser/types/deck'
import type { DeckFace } from '../deckChoice'
import { useServerText } from '../i18n'
import '../browser/browser.css'

// A deck loaded from the lobby, looked over before it is played: kept in your decks
// only when asked, so a deck to try needs no trip to the decks page.
const emit = defineEmits<{ use: [face: DeckFace, list: DeckList]; saved: [deck: Deck]; cancel: [] }>()
const tr = useServerText()

const loaded = ref<{ preview: DeckPreview; request: ImportRequest } | null>(null)
const save = ref(false)
const busy = ref(false)
const error = ref<string | null>(null)

const face = computed((): DeckFace | null => {
  const preview = loaded.value?.preview
  if (!preview) return null
  const names = preview.deck.heroes.map((s) => s.card?.name ?? s.sourceCode)
  return { key: 'loaded', sphere: null, name: preview.deck.name, heroCodes: preview.heroCodes, detail: names.join(', ') }
})

const use = async () => {
  const { preview, request } = loaded.value!
  if (!save.value) return emit('use', face.value!, preview.list!)
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
    <div v-if="!loaded" class="card-browser bare">
      <DeckImport :keep="false" @loaded="(preview, request) => (loaded = { preview, request })" />
    </div>
    <template v-else-if="face">
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
      <button type="button" class="secondary" @click="loaded ? (loaded = null) : emit('cancel')">{{ $t('common.back') }}</button>
      <button v-if="loaded" type="button" class="primary" :disabled="busy || !loaded.preview.list" @click="use">{{ $t('newGame.loader.use') }}</button>
    </div>
  </div>
</template>

<style scoped>
.deck-loader { display: grid; gap: 12px; }
/* the browser's tokens for the import row, not its page background */
.bare { background: none; }
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
