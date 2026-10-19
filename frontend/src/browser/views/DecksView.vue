<script setup lang="ts">
/**
 * A user's decks, and the prompt to load one.
 *
 * With no decks the import panel is the page; with decks it sits beside them,
 * because loading another is the main thing anyone does here.
 */
import '@browser/browser.css'
import { onMounted, ref } from 'vue'
import { useRouter } from 'vue-router'
import DeckImport from '@browser/components/DeckImport.vue'
import { useDeckStore, messageOf } from '@browser/stores/decks'
import type { Deck, DeckSummary } from '@browser/types/deck'

const router = useRouter()
const decks = useDeckStore()

const removing = ref<number | null>(null)
const removeError = ref<string | null>(null)

onMounted(() => decks.load())

function onImported(deck: Deck): void {
  void router.push({ name: 'deck', params: { id: deck.id } })
}

async function remove(deck: DeckSummary): Promise<void> {
  if (removing.value !== null) return
  removing.value = deck.id
  removeError.value = null
  try {
    await decks.remove(deck.id)
  } catch (err) {
    removeError.value = messageOf(err)
  } finally {
    removing.value = null
  }
}
</script>

<template>
  <main class="page card-browser">
    <header class="page__head">
      <h1>Decks</h1>
      <p v-if="decks.decks.length" class="muted">
        {{ decks.decks.length }} {{ decks.decks.length === 1 ? 'deck' : 'decks' }}
      </p>
    </header>

    <div class="layout" :class="{ 'layout--empty': decks.isEmpty }">
      <DeckImport class="layout__import" @imported="onImported" />

      <div class="layout__list">
        <p v-if="decks.state === 'loading'" class="notice">Loading…</p>
        <p v-else-if="decks.state === 'error'" class="notice notice--error">{{ decks.error }}</p>

        <p v-else-if="decks.isEmpty" class="notice">
          Nothing here yet. Import a deck to get started — the fifty sample decks in
          <code>backend/data/decks</code> are all built from the Core Set.
        </p>

        <ul v-else class="decks">
          <li v-for="deck in decks.decks" :key="deck.id" class="deck">
            <RouterLink class="deck__link" :to="{ name: 'deck', params: { id: deck.id } }">
              <h2 class="deck__name truncate">{{ deck.name }}</h2>
              <p class="deck__heroes truncate">
                {{ deck.heroNames.length ? deck.heroNames.join(' · ') : 'No heroes' }}
              </p>
              <p class="deck__meta">
                <span>{{ deck.cardCount }} cards</span>
                <span v-if="deck.startingThreat !== null">
                  starting threat {{ deck.startingThreat }}
                </span>
                <span v-if="deck.source === 'ringsdb'" class="chip">RingsDB</span>
                <span v-if="deck.unresolvedCount" class="chip chip--warn">
                  {{ deck.unresolvedCount }} unknown
                </span>
              </p>
            </RouterLink>

            <button
              type="button"
              class="button button--ghost deck__remove"
              :disabled="removing === deck.id"
              :title="`Delete ${deck.name}`"
              @click="remove(deck)"
            >
              <span aria-hidden="true">&#10005;</span>
              <span class="visually-hidden">Delete {{ deck.name }}</span>
            </button>
          </li>
        </ul>

        <p v-if="removeError" class="notice notice--error" role="alert">{{ removeError }}</p>
      </div>
    </div>
  </main>
</template>

<style scoped>
.page {
  max-width: 66rem;
  margin-inline: auto;
  padding: var(--space-5) var(--space-4) var(--space-6);
}

.page__head {
  display: flex;
  align-items: baseline;
  gap: var(--space-3);
  margin-bottom: var(--space-5);
}

h1 {
  font-family: var(--font-display);
  font-size: var(--text-xl);
}

.layout {
  display: grid;
  grid-template-columns: 1fr 24rem;
  gap: var(--space-5);
  align-items: start;
}

.layout__import {
  grid-column: 2;
  grid-row: 1;
}

.layout__list {
  grid-column: 1;
  grid-row: 1;
}

/* With nothing to list, the prompt is the page rather than a sidebar to it. */
.layout--empty {
  grid-template-columns: minmax(0, 34rem);
  justify-content: center;
}

.layout--empty .layout__import,
.layout--empty .layout__list {
  grid-column: 1;
  grid-row: auto;
}

.decks {
  display: flex;
  flex-direction: column;
  gap: var(--space-2);
}

.deck {
  display: flex;
  align-items: stretch;
  border: 1px solid var(--c-border);
  border-radius: var(--radius);
  background: var(--c-surface);
  transition: border-color var(--transition);
}

.deck:hover {
  border-color: var(--c-border-strong);
}

.deck__link {
  flex: 1;
  min-width: 0;
  padding: var(--space-3) var(--space-4);
}

.deck__name {
  font-family: var(--font-display);
  font-size: var(--text-base);
}

.deck__heroes {
  color: var(--c-text-muted);
  font-size: var(--text-sm);
}

.deck__meta {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: var(--space-2);
  margin-top: var(--space-1);
  color: var(--c-text-faint);
  font-size: var(--text-xs);
}

.chip--warn {
  color: var(--c-danger);
}

.deck__remove {
  align-self: center;
  margin-right: var(--space-2);
  color: var(--c-text-faint);
}

.deck__remove:hover:not(:disabled) {
  color: var(--c-danger);
}

.notice {
  padding: var(--space-5);
  border: 1px dashed var(--c-border);
  border-radius: var(--radius);
  color: var(--c-text-muted);
  font-size: var(--text-sm);
  text-align: center;
  text-wrap: balance;
}

.notice--error {
  border-style: solid;
  border-color: var(--c-danger);
  color: var(--c-danger);
}

code {
  font-family: var(--font-mono);
  font-size: 0.9em;
}

@media (width < 60rem) {
  .layout {
    grid-template-columns: minmax(0, 1fr);
  }

  .layout__import,
  .layout__list {
    grid-column: 1;
  }

  .layout__import {
    grid-row: 1;
  }

  .layout__list {
    grid-row: 2;
  }
}
</style>
