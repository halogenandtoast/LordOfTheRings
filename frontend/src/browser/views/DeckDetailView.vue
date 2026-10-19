<script setup lang="ts">
/**
 * One deck: its heroes as cards, its draw deck grouped by type.
 *
 * The deck arrives from the API with every card already attached, so this page
 * does not need the card index and renders before it has loaded.
 */
import '@browser/browser.css'
import { computed, ref, watch } from 'vue'
import { useRoute } from 'vue-router'
import { useI18n } from 'vue-i18n'
import CardImage from '@browser/components/CardImage.vue'
import DeckSection from '@browser/components/DeckSection.vue'
import { fetchDeck } from '@browser/lib/api'
import { messageOf } from '@browser/stores/decks'
import { computedThreat, drawDeckSize, unresolvedSlots, type Deck, type Slot } from '@browser/types/deck'
import type { CardType } from '@browser/types/card'

const route = useRoute()
const { t, te } = useI18n()

const deck = ref<Deck | null>(null)
const error = ref<string | null>(null)
const loading = ref(true)

watch(
  () => route.params.id,
  async (raw) => {
    const id = Number(raw)
    loading.value = true
    error.value = null
    deck.value = null
    try {
      deck.value = await fetchDeck(id)
    } catch (err) {
      error.value = messageOf(err)
    } finally {
      loading.value = false
    }
  },
  { immediate: true },
)

/** The order a deck list is conventionally written in. */
const TYPE_ORDER: CardType[] = ['ally', 'attachment', 'event', 'player-side-quest', 'contract']

/** The draw deck, split by card type, in that order, with anything else last. */
const byType = computed(() => {
  if (!deck.value) return []
  const groups = new Map<string, Slot[]>()
  for (const slot of deck.value.slots) {
    const type = slot.card?.type ?? 'other'
    const existing = groups.get(type)
    if (existing) existing.push(slot)
    else groups.set(type, [slot])
  }
  const rank = (type: string) => {
    const i = TYPE_ORDER.indexOf(type as CardType)
    return i === -1 ? TYPE_ORDER.length : i
  }
  return [...groups.entries()]
    .sort((a, b) => rank(a[0]) - rank(b[0]) || a[0].localeCompare(b[0]))
    .map(([type, slots]) => ({
      type,
      title: te(`browser.cardTypes.${type}`) ? t(`browser.cardTypes.${type}`) : t('browser.deck.other'),
      slots,
    }))
})

const threat = computed(() => {
  if (!deck.value) return null
  return deck.value.startingThreat ?? computedThreat(deck.value)
})

const unknown = computed(() => (deck.value ? unresolvedSlots(deck.value) : []))
const size = computed(() => (deck.value ? drawDeckSize(deck.value) : 0))
</script>

<template>
  <main class="page card-browser">
    <RouterLink class="button button--ghost back" to="/decks">&#8592; {{ $t('browser.deck.allDecks') }}</RouterLink>

    <p v-if="loading" class="notice">{{ $t('browser.common.loading') }}</p>
    <p v-else-if="error" class="notice notice--error">{{ error }}</p>

    <template v-else-if="deck">
      <header class="head">
        <h1>{{ deck.name }}</h1>
        <p class="head__meta">
          <span>{{ $t('browser.common.cardCount', size) }}</span>
          <span v-if="threat !== null">{{ $t('browser.common.startingThreat', { threat }) }}</span>
          <a
            v-if="deck.sourceUrl"
            class="head__source"
            :href="deck.sourceUrl"
            target="_blank"
            rel="noreferrer noopener"
          >
            {{ $t('browser.deck.onRingsdb') }} &#8599;
          </a>
        </p>
        <p v-if="size < 50" class="head__warning">
          {{ $t('browser.deck.tooSmall', { count: size }) }}
        </p>
      </header>

      <p v-if="unknown.length" class="notice notice--error">
        {{ $t('browser.deck.unresolved', unknown.length) }}
      </p>

      <section v-if="deck.heroes.length" class="heroes">
        <article v-for="slot in deck.heroes" :key="slot.sourceCode" class="hero">
          <CardImage v-if="slot.card" :card="slot.card" size="thumb" />
          <div v-else class="hero__missing">{{ slot.sourceCode }}</div>
          <h2 class="hero__name truncate">{{ slot.card?.name ?? $t('browser.common.unknownCard') }}</h2>
          <p class="hero__sphere muted">
            {{ slot.card?.sphere ? $t(`browser.spheres.${slot.card.sphere}`) : '—' }}
          </p>
        </article>
      </section>

      <div class="columns">
        <DeckSection
          v-for="group in byType"
          :key="group.type"
          :title="group.title"
          :slots="group.slots"
        />
        <DeckSection
          v-if="deck.sideslots.length"
          :title="$t('browser.deck.sideboard')"
          :slots="deck.sideslots"
          class="sideboard"
        />
      </div>

      <p v-if="deck.description" class="description">{{ deck.description }}</p>
    </template>
  </main>
</template>

<style scoped>
.page {
  max-width: 60rem;
  margin-inline: auto;
  padding: var(--space-4) var(--space-4) var(--space-6);
}

.back {
  margin-bottom: var(--space-4);
}

.head {
  margin-bottom: var(--space-5);
}

h1 {
  font-family: var(--font-display);
  font-size: var(--text-xl);
  text-wrap: balance;
}

.head__meta {
  display: flex;
  flex-wrap: wrap;
  gap: var(--space-3);
  margin-top: var(--space-1);
  color: var(--c-text-faint);
  font-size: var(--text-sm);
}

.head__source:hover {
  color: var(--c-accent);
}

.head__warning {
  margin-top: var(--space-2);
  color: var(--c-text-muted);
  font-size: var(--text-sm);
}

.heroes {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(9rem, 1fr));
  gap: var(--space-4);
  margin-bottom: var(--space-6);
}

.hero__missing {
  display: flex;
  align-items: center;
  justify-content: center;
  aspect-ratio: var(--card-aspect-portrait);
  border: 1px dashed var(--c-danger);
  border-radius: var(--radius-card);
  color: var(--c-danger);
  font-family: var(--font-mono);
  font-size: var(--text-sm);
}

.hero__name {
  margin-top: var(--space-2);
  font-family: var(--font-display);
  font-size: var(--text-base);
}

.hero__sphere {
  font-size: var(--text-xs);
  text-transform: capitalize;
}

/*
 * Sections flow into as many columns as fit, each kept whole, so a deck list
 * reads down a column the way it is written on paper.
 */
.columns {
  columns: 18rem;
  column-gap: var(--space-5);
}

.columns > * {
  break-inside: avoid;
  margin-bottom: var(--space-5);
}

.sideboard {
  opacity: 0.75;
}

.description {
  margin-top: var(--space-5);
  padding-top: var(--space-4);
  border-top: 1px solid var(--c-border);
  color: var(--c-text-muted);
  font-size: var(--text-sm);
  white-space: pre-wrap;
}

.notice {
  padding: var(--space-4);
  border: 1px dashed var(--c-border);
  border-radius: var(--radius);
  color: var(--c-text-muted);
  font-size: var(--text-sm);
  text-align: center;
}

.notice--error {
  border-style: solid;
  border-color: var(--c-danger);
  color: var(--c-danger);
  text-align: left;
}
</style>
