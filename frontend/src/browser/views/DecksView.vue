<script setup lang="ts">
/**
 * A user's decks, and the row to load another above them, found by name, hero or
 * sphere and sorted, by default, as they were last played.
 */
import '@browser/browser.css'
import { computed, onMounted, ref } from 'vue'
import { useRouter } from 'vue-router'
import DeckImport from '@browser/components/DeckImport.vue'
import GameIcon from '@browser/components/GameIcon.vue'
import TrashIcon from '../../components/TrashIcon.vue'
import { useDeckStore, messageOf } from '@browser/stores/decks'
import type { IconName } from '@browser/lib/card-text'
import { PLAYER_SPHERES, type Sphere } from '@browser/types/card'
import type { Deck, DeckSummary } from '@browser/types/deck'
import { useServerText } from '../../i18n'

const tr = useServerText()

const router = useRouter()
const decks = useDeckStore()

const removing = ref<number | null>(null)
const removeError = ref<string | null>(null)

// afresh each visit: a deck saved from a lobby, or one just played, changes the list
onMounted(() => decks.load(true))

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

const search = ref('')
// a deck shows when its heroes cover every sphere chosen
const spheres = ref<Sphere[]>([])
const SORTS = ['used', 'name', 'threat', 'imported'] as const
const sort = ref<(typeof SORTS)[number]>('used')
const reversed = ref(false)

const toggleSphere = (sphere: Sphere) => {
  spheres.value = spheres.value.includes(sphere) ? spheres.value.filter((s) => s !== sphere) : [...spheres.value, sphere]
}

const time = (at: string | null) => (at ? Date.parse(at) : 0)
// each sort's natural order: the latest first, names and threat ascending
const compare: Record<(typeof SORTS)[number], (a: DeckSummary, b: DeckSummary) => number> = {
  used: (a, b) => time(b.usedAt ?? b.updatedAt) - time(a.usedAt ?? a.updatedAt),
  name: (a, b) => a.name.localeCompare(b.name),
  threat: (a, b) => (a.startingThreat ?? Infinity) - (b.startingThreat ?? Infinity),
  imported: (a, b) => time(b.updatedAt) - time(a.updatedAt),
}

const shown = computed(() => {
  const words = search.value.trim().toLowerCase()
  const matching = decks.decks.filter(
    (d) =>
      (!words || [d.name, ...d.heroes.map((h) => h.name)].some((n) => n.toLowerCase().includes(words))) &&
      spheres.value.every((s) => d.heroes.some((h) => h.sphere === s)),
  )
  const sorted = matching.sort(compare[sort.value])
  return reversed.value ? sorted.reverse() : sorted
})
</script>

<template>
  <main class="page card-browser">
    <header class="page__head">
      <h1>{{ $t('browser.decks.title') }}</h1>
      <p v-if="decks.decks.length" class="muted">
        {{ $t('browser.decks.count', decks.decks.length) }}
      </p>
    </header>

    <DeckImport class="import" @imported="onImported" />

    <p v-if="decks.state === 'loading'" class="notice">{{ $t('browser.common.loading') }}</p>
    <p v-else-if="decks.state === 'error'" class="notice notice--error">{{ tr(decks.error) }}</p>

    <i18n-t v-else-if="decks.isEmpty" keypath="browser.decks.empty" scope="global" tag="p" class="notice">
      <template #path><code>backend/data/decks</code></template>
    </i18n-t>

    <template v-else>
      <div class="toolbar">
        <input
          v-model="search"
          class="input toolbar__search"
          type="search"
          :placeholder="$t('browser.decks.search')"
          :aria-label="$t('browser.decks.search')"
          autocomplete="off"
        />
        <div class="toolbar__spheres" role="group" :aria-label="$t('browser.decks.spheres')">
          <button
            v-for="sphere in PLAYER_SPHERES"
            :key="sphere"
            type="button"
            class="button button--ghost"
            :aria-pressed="spheres.includes(sphere)"
            @click="toggleSphere(sphere)"
          >
            <GameIcon :name="sphere" />
          </button>
        </div>
        <label>
          <span class="visually-hidden">{{ $t('browser.browse.sortBy') }}</span>
          <select v-model="sort" class="input">
            <option v-for="value in SORTS" :key="value" :value="value">{{ $t(`browser.decks.sort.${value}`) }}</option>
          </select>
        </label>
        <button
          type="button"
          class="button button--ghost"
          :aria-pressed="reversed"
          :title="$t('browser.browse.reverseSort')"
          @click="reversed = !reversed"
        >
          <span aria-hidden="true">{{ reversed ? '↑' : '↓' }}</span>
          <span class="visually-hidden">{{ $t('browser.browse.reverseSort') }}</span>
        </button>
      </div>

      <p v-if="!shown.length" class="notice">{{ $t('browser.decks.noneMatch') }}</p>
      <ul v-else class="decks">
        <li v-for="deck in shown" :key="deck.id" class="deck">
          <RouterLink class="deck__link" :to="{ name: 'deck', params: { id: deck.id } }">
            <!-- the heroes' art side by side, as a deck tile shows them -->
            <span class="deck__art" aria-hidden="true">
              <span v-for="hero in deck.heroes" :key="hero.code" :style="{ backgroundImage: `url(/images/cards/${hero.code}.jpg)` }" />
            </span>
            <div class="deck__body">
              <h2 class="deck__name truncate">{{ deck.name }}</h2>
              <p class="deck__heroes">
                <span v-for="hero in deck.heroes" :key="hero.code" class="hero" :style="hero.sphere ? { color: `var(--c-${hero.sphere})` } : undefined">
                  <GameIcon v-if="hero.sphere" :name="hero.sphere as IconName" />{{ hero.name }}
                </span>
                <span v-if="!deck.heroes.length">{{ $t('browser.decks.noHeroes') }}</span>
              </p>
              <p class="deck__meta">
                <span>{{ $t('browser.common.cardCount', deck.cardCount) }}</span>
                <span v-if="deck.startingThreat !== null">
                  {{ $t('browser.common.startingThreat', { threat: deck.startingThreat }) }}
                </span>
                <span v-if="deck.source === 'ringsdb'" class="chip">RingsDB</span>
                <span v-if="deck.unresolvedCount" class="chip chip--warn">
                  {{ $t('browser.decks.unknownCount', { count: deck.unresolvedCount }) }}
                </span>
              </p>
            </div>
          </RouterLink>

          <button
            type="button"
            class="deck__remove"
            :disabled="removing === deck.id"
            :title="$t('browser.decks.delete', { name: deck.name })"
            @click="remove(deck)"
          >
            <TrashIcon />
            <span class="visually-hidden">{{ $t('browser.decks.delete', { name: deck.name }) }}</span>
          </button>
        </li>
      </ul>

      <p v-if="removeError" class="notice notice--error" role="alert">{{ tr(removeError) }}</p>
    </template>
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

.import {
  margin-bottom: var(--space-4);
}

.toolbar {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: var(--space-2);
  margin-bottom: var(--space-3);
}

.toolbar__search {
  flex: 1 1 14rem;
}

.toolbar__spheres {
  display: flex;
  gap: 2px;
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
  display: flex;
  min-width: 0;
}

.deck__art {
  flex: 0 0 11rem;
  display: grid;
  grid-auto-flow: column;
  grid-auto-columns: 1fr;
  border-radius: var(--radius) 0 0 var(--radius);
  overflow: hidden;
  background: var(--c-surface-2, rgb(0 0 0 / 0.2));
}

/* the art sits in the top half of a card, right of the stats; sized by the row's
   height, as rows are wider than a deck tile is tall */
.deck__art > span {
  background-size: auto 200%;
  background-position: 62% 0;
}

.deck__art > span + span {
  box-shadow: -1px 0 0 rgb(0 0 0 / 0.5);
}

.deck__body {
  flex: 1;
  min-width: 0;
  padding: var(--space-3) var(--space-4);
}

.deck__name {
  font-family: var(--font-display);
  font-size: var(--text-base);
}

.deck__heroes {
  display: flex;
  flex-wrap: wrap;
  gap: 2px var(--space-3);
  color: var(--c-text-muted);
  font-size: var(--text-sm);
}

/* each hero in its sphere's colour, its icon before it */
.hero {
  display: inline-flex;
  align-items: center;
  gap: 4px;
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

/* as a game row's delete */
.deck__remove {
  display: flex;
  align-self: center;
  margin-right: var(--space-3);
  padding: 4px;
  border: 0;
  background: none;
  color: var(--c-text-faint);
  cursor: pointer;
}

.deck__remove svg {
  width: 18px;
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
</style>
