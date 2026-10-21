<script setup lang="ts">
/**
 * The two ways a deck gets in, side by side in one row: a RingsDB link, or a JSON
 * file, chosen or dropped on the row.
 *
 * Both end at the same endpoint. A link is handed to the server as a link —
 * the browser cannot fetch ringsdb.com itself, and the server will only follow
 * a URL it recognises — while a file is read here and posted as its contents,
 * so no upload endpoint is needed.
 */
import { ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { useDeckStore, messageOf } from '@browser/stores/decks'
import type { Deck, DeckPreview, ImportRequest } from '@browser/types/deck'
import { previewDeck } from '@browser/lib/api'
import { useServerText } from '../../i18n'

const tr = useServerText()


// kept in your decks, or only loaded, to look over before playing it
const { keep = true } = defineProps<{ keep?: boolean }>()
const emit = defineEmits<{ imported: [deck: Deck]; loaded: [preview: DeckPreview, request: ImportRequest] }>()

const decks = useDeckStore()
const { t } = useI18n()

const url = ref('')
const busy = ref(false)
const error = ref<string | null>(null)
const dragging = ref(false)
const fileInput = ref<HTMLInputElement | null>(null)

async function run(request: ImportRequest): Promise<void> {
  if (busy.value) return
  busy.value = true
  error.value = null
  try {
    if (keep) emit('imported', await decks.importDeck(request))
    else emit('loaded', await previewDeck(request), request)
    url.value = ''
  } catch (err) {
    error.value = messageOf(err)
  } finally {
    busy.value = false
  }
}

function importFromUrl(): void {
  if (url.value.trim()) void run({ url: url.value.trim() })
}

async function importFile(file: File): Promise<void> {
  let parsed: unknown
  try {
    parsed = JSON.parse(await file.text())
  } catch {
    error.value = t('browser.deckImport.invalidJson', { name: file.name })
    return
  }
  await run({ deck: parsed })
}

function onPick(event: Event): void {
  const file = (event.target as HTMLInputElement).files?.[0]
  if (file) void importFile(file)
  // Clear the input so picking the same file twice still fires.
  if (fileInput.value) fileInput.value.value = ''
}

function onDrop(event: DragEvent): void {
  dragging.value = false
  const file = event.dataTransfer?.files?.[0]
  if (file) void importFile(file)
}
</script>

<template>
  <div class="import">
    <form
      class="row"
      :class="{ 'row--over': dragging }"
      @submit.prevent="importFromUrl"
      @dragover.prevent="dragging = true"
      @dragleave="dragging = false"
      @drop.prevent="onDrop"
    >
      <label class="visually-hidden" for="deck-url">{{ $t('browser.deckImport.urlLabel') }}</label>
      <input
        id="deck-url"
        v-model="url"
        class="input"
        type="url"
        inputmode="url"
        :placeholder="$t('browser.deckImport.placeholder')"
        :disabled="busy"
      />
      <button class="button button--primary" type="submit" :disabled="busy || !url.trim()">
        {{ busy ? $t('browser.common.loading') : $t('browser.deckImport.import') }}
      </button>
      <label class="button file" :title="$t('browser.deckImport.fileHint')">
        <input
          ref="fileInput"
          class="visually-hidden"
          type="file"
          accept="application/json,.json"
          :disabled="busy"
          @change="onPick"
        />
        {{ $t('browser.deckImport.file') }}
      </label>
    </form>
    <p v-if="error" class="error" role="alert">{{ tr(error) }}</p>
  </div>
</template>

<style scoped>
.import {
  display: grid;
  gap: var(--space-2);
}

.row {
  display: flex;
  gap: var(--space-2);
  padding: var(--space-2);
  border: 1px dashed transparent;
  border-radius: var(--radius-lg);
  background: var(--c-surface);
  transition: border-color var(--transition);
}

/* a file dropped anywhere on the row loads it */
.row--over {
  border-color: var(--c-accent);
}

.row .input {
  flex: 1;
  min-width: 0;
}

.button--primary {
  background: var(--c-accent);
  border-color: var(--c-accent);
  color: var(--c-accent-contrast);
  font-weight: 600;
}

.button--primary:hover:not(:disabled) {
  background: var(--c-accent);
  border-color: var(--c-focus);
}

.file {
  cursor: pointer;
  white-space: nowrap;
}

.file:focus-within {
  outline: 2px solid var(--c-focus);
  outline-offset: 2px;
}

.error {
  padding: var(--space-2) var(--space-3);
  border: 1px solid var(--c-danger);
  border-radius: var(--radius);
  color: var(--c-danger);
  font-size: var(--text-sm);
}
</style>
