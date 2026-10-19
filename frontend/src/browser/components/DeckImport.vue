<script setup lang="ts">
/**
 * The two ways a deck gets in: a RingsDB link, or a JSON file.
 *
 * Both end at the same endpoint. A link is handed to the server as a link —
 * the browser cannot fetch ringsdb.com itself, and the server will only follow
 * a URL it recognises — while a file is read here and posted as its contents,
 * so no upload endpoint is needed.
 */
import { ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { useDeckStore, messageOf } from '@browser/stores/decks'
import type { Deck } from '@browser/types/deck'

const emit = defineEmits<{ imported: [deck: Deck] }>()

const decks = useDeckStore()
const { t } = useI18n()

const url = ref('')
const busy = ref(false)
const error = ref<string | null>(null)
const dragging = ref(false)
const fileInput = ref<HTMLInputElement | null>(null)

async function run(request: Parameters<typeof decks.importDeck>[0]): Promise<void> {
  if (busy.value) return
  busy.value = true
  error.value = null
  try {
    emit('imported', await decks.importDeck(request))
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
  <section class="import">
    <h2 class="import__title">{{ $t('browser.deckImport.title') }}</h2>
    <p class="import__lead muted">
      {{ $t('browser.deckImport.lead') }}
    </p>

    <form class="from-url" @submit.prevent="importFromUrl">
      <label class="visually-hidden" for="deck-url">{{ $t('browser.deckImport.urlLabel') }}</label>
      <input
        id="deck-url"
        v-model="url"
        class="input"
        type="url"
        inputmode="url"
        placeholder="https://ringsdb.com/decklist/view/54349"
        :disabled="busy"
      />
      <button class="button button--primary" type="submit" :disabled="busy || !url.trim()">
        {{ busy ? $t('browser.common.loading') : $t('browser.deckImport.import') }}
      </button>
    </form>

    <p class="or" aria-hidden="true"><span>{{ $t('browser.deckImport.or') }}</span></p>

    <label
      class="from-file"
      :class="{ 'from-file--over': dragging }"
      @dragover.prevent="dragging = true"
      @dragleave="dragging = false"
      @drop.prevent="onDrop"
    >
      <input
        ref="fileInput"
        class="visually-hidden"
        type="file"
        accept="application/json,.json"
        :disabled="busy"
        @change="onPick"
      />
      <span class="from-file__mark" aria-hidden="true">&#8681;</span>
      <span class="from-file__label">{{ $t('browser.deckImport.fileLabel') }}</span>
      <span class="from-file__hint muted">{{ $t('browser.deckImport.fileHint') }}</span>
    </label>

    <p v-if="error" class="error" role="alert">{{ error }}</p>
  </section>
</template>

<style scoped>
.import {
  display: flex;
  flex-direction: column;
  gap: var(--space-3);
  padding: var(--space-5);
  border: 1px solid var(--c-border);
  border-radius: var(--radius-lg);
  background: var(--c-surface);
}

.import__title {
  font-family: var(--font-display);
  font-size: var(--text-lg);
}

.import__lead {
  margin-top: calc(var(--space-2) * -1);
  font-size: var(--text-sm);
}

.from-url {
  display: flex;
  gap: var(--space-2);
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

/* A rule with the word sitting in a gap in it. */
.or {
  display: flex;
  align-items: center;
  gap: var(--space-3);
  color: var(--c-text-faint);
  font-size: var(--text-xs);
  text-transform: uppercase;
  letter-spacing: 0.12em;
}

.or::before,
.or::after {
  content: '';
  flex: 1;
  height: 1px;
  background: var(--c-border);
}

.from-file {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: var(--space-1);
  padding: var(--space-5);
  border: 1px dashed var(--c-border-strong);
  border-radius: var(--radius);
  background: var(--c-surface-2);
  text-align: center;
  cursor: pointer;
  transition:
    border-color var(--transition),
    background var(--transition);
}

.from-file:hover,
.from-file--over {
  border-color: var(--c-accent);
  background: var(--c-surface-3);
}

.from-file:focus-within {
  outline: 2px solid var(--c-focus);
  outline-offset: 2px;
}

.from-file__mark {
  color: var(--c-accent);
  font-size: var(--text-xl);
  line-height: 1;
}

.from-file__label {
  font-size: var(--text-sm);
}

.from-file__hint {
  font-size: var(--text-xs);
}

.error {
  padding: var(--space-2) var(--space-3);
  border: 1px solid var(--c-danger);
  border-radius: var(--radius);
  color: var(--c-danger);
  font-size: var(--text-sm);
}

@media (width < 34rem) {
  .from-url {
    flex-direction: column;
  }
}
</style>
