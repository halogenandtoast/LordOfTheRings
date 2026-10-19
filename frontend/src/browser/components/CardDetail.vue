<script setup lang="ts">
/**
 * Everything known about one card: the scan, its statistics, its rules text,
 * and where the data came from.
 */
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import CardImage from './CardImage.vue'
import CardStats from './CardStats.vue'
import CardText from './CardText.vue'
import GameIcon from './GameIcon.vue'
import type { Card } from '@browser/types/card'
import { primaryCost } from '@browser/types/card'
import type { IconName } from '@browser/lib/card-text'

const { card } = defineProps<{ card: Card }>()
const { t } = useI18n()

/**
 * Which image is showing. Errata reprints and quest stages each get their own,
 * and the dialog drives this with the up and down arrows, so it is a model
 * rather than local state.
 */
const imageIndex = defineModel<number>('imageIndex', { default: 0 })

const activeImage = computed(() => card.images[imageIndex.value] ?? card.images[0])

const imageLabels = computed(() =>
  card.images.map((image) => {
    const parts: string[] = []
    if (image.stage !== null) parts.push(t('browser.card.imageStage', { stage: image.stage }))
    parts.push(t('browser.card.imageSide', { face: image.face.toUpperCase() }))
    if (image.variant) parts.push(image.variant)
    return parts.join(' · ')
  }),
)

const cost = computed(() => primaryCost(card))
const typeLabel = computed(() =>
  card.type ? t(`browser.cardTypes.${card.type}`) : (card.typeName ?? t('browser.card.unknownType')),
)
const sphereLabel = computed(() => (card.sphere ? t(`browser.spheres.${card.sphere}`) : null))

const sphereIcon = computed<IconName | null>(() =>
  card.sphere &&
  (
    ['leadership', 'lore', 'spirit', 'tactics', 'baggins', 'fellowship', 'neutral'] as const
  ).includes(card.sphere as never)
    ? (card.sphere as IconName)
    : null,
)

const facts = computed(() => {
  const rows: { label: string; value: string }[] = []
  rows.push({ label: t('browser.card.facts.set'), value: card.setName })
  if (card.packName !== card.setName) rows.push({ label: t('browser.card.facts.pack'), value: card.packName })
  if (card.cycleName !== card.packName) rows.push({ label: t('browser.card.facts.cycle'), value: card.cycleName })
  if (card.number !== null) {
    rows.push({ label: t('browser.card.facts.number'), value: String(card.number).padStart(3, '0') })
  }
  rows.push({ label: t('browser.card.facts.section'), value: t(`browser.sections.${card.section}`) })
  if (card.encounterSet) rows.push({ label: t('browser.card.facts.encounterSet'), value: card.encounterSet })
  if (card.quantity !== null) rows.push({ label: t('browser.card.facts.quantity'), value: String(card.quantity) })
  if (card.deckLimit !== null) rows.push({ label: t('browser.card.facts.deckLimit'), value: String(card.deckLimit) })
  if (card.illustrator) rows.push({ label: t('browser.card.facts.illustrator'), value: card.illustrator })
  if (card.isBackOf) rows.push({ label: t('browser.card.facts.reverseOf'), value: card.isBackOf })
  return rows
})
</script>

<template>
  <div class="detail-container">
    <article
      class="detail"
      :data-sphere="card.sphere ?? undefined"
      :data-type="card.type ?? undefined"
    >
      <div class="detail__art">
        <CardImage :card="card" :image="activeImage" size="full" eager />

        <div v-if="card.images.length > 1" class="faces" role="tablist" :aria-label="$t('browser.card.faces')">
          <button
            v-for="(label, i) in imageLabels"
            :key="i"
            type="button"
            class="button"
            role="tab"
            :aria-selected="i === imageIndex"
            :class="{ 'is-active': i === imageIndex }"
            @click="imageIndex = i"
          >
            {{ label }}
          </button>
        </div>
      </div>

      <div class="detail__body stack">
        <header class="head">
          <h2 class="head__name">
            <span v-if="card.unique" class="unique" :aria-label="$t('browser.common.unique')">&#9670;</span>
            {{ card.name }}
          </h2>

          <p class="head__line">
            <span v-if="cost !== null" class="head__cost">
              {{ $t(card.type === 'hero' ? 'browser.card.threat' : 'browser.card.cost') }} {{ cost }}
            </span>
            <span v-if="sphereLabel" class="head__sphere">
              <GameIcon v-if="sphereIcon" :name="sphereIcon" />
              {{ sphereLabel }}
            </span>
            <span class="head__type">{{ typeLabel }}</span>
            <span v-if="card.stage !== null" class="head__stage">{{ $t('browser.card.stage', { stage: card.stage }) }}</span>
          </p>

          <p v-if="card.traits.length" class="traits">{{ card.traits.join('. ') }}.</p>
        </header>

        <CardStats :card="card" />

        <ul v-if="card.keywords.length" class="keywords">
          <li v-for="keyword in card.keywords" :key="keyword" class="chip">{{ keyword }}</li>
        </ul>

        <section v-if="card.setupText" class="block">
          <h3 class="block__title">{{ $t('browser.card.setup') }}</h3>
          <CardText :text="card.setupText" />
        </section>

        <section v-if="card.text" class="block">
          <CardText :text="card.text" />
        </section>

        <section v-if="card.shadow" class="block block--shadow">
          <CardText :text="card.shadow" />
        </section>

        <section v-if="card.back" class="block block--back">
          <h3 class="block__title">
            {{ $t('browser.card.reverse') }}{{
              card.back.name && card.back.name !== card.name ? ` · ${card.back.name}` : ''
            }}
          </h3>
          <p v-if="card.back.traits.length" class="traits">{{ card.back.traits.join('. ') }}.</p>
          <CardText v-if="card.back.text" :text="card.back.text" />
          <p v-if="card.back.questPoints !== null" class="back-stat">
            {{ $t('browser.card.questPoints', { count: card.back.questPoints }) }}
          </p>
        </section>

        <p v-if="card.flavor" class="flavor"><CardText :text="card.flavor" /></p>

        <dl class="facts">
          <template v-for="fact in facts" :key="fact.label">
            <dt>{{ fact.label }}</dt>
            <dd>{{ fact.value }}</dd>
          </template>
        </dl>

        <p v-if="card.ringsdbUrl" class="links">
          <a class="button button--ghost" :href="card.ringsdbUrl" target="_blank" rel="noreferrer">
            {{ $t('browser.card.viewOnRingsdb') }} &#8599;
          </a>
        </p>
      </div>
    </article>
  </div>
</template>

<style scoped>
/*
 * The card sits in two very different boxes: a wide standalone page, and the
 * narrow side panel of the browser. A container query lets it stack for the
 * panel without the viewport — wide in both cases — misleading it. The query
 * has to name an ancestor, since an element cannot size-query itself, so the
 * wrapper exists only to be that container.
 */
.detail-container {
  container-type: inline-size;
}

.detail {
  display: grid;
  grid-template-columns: minmax(0, 20rem) minmax(0, 1fr);
  gap: var(--space-5);
  align-items: start;
}

.detail__art {
  position: sticky;
  top: var(--space-4);
  display: flex;
  flex-direction: column;
  gap: var(--space-3);
}

.faces {
  display: flex;
  flex-wrap: wrap;
  gap: var(--space-2);
}

.head__name {
  display: flex;
  align-items: baseline;
  gap: 0.3em;
  font-family: var(--font-display);
  font-size: var(--text-xl);
}

.unique {
  color: var(--sphere-color, var(--c-accent));
  font-size: 0.6em;
}

.head__line {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: var(--space-3);
  margin-top: var(--space-1);
  font-size: var(--text-sm);
  color: var(--c-text-muted);
}

.head__sphere {
  display: inline-flex;
  align-items: center;
  gap: 0.3em;
  color: var(--sphere-color, inherit);
}

.head__type {
  color: var(--type-color, var(--c-text-muted));
}

.traits {
  margin-top: var(--space-2);
  font-style: italic;
  font-weight: 600;
  color: var(--c-text-muted);
  font-size: var(--text-sm);
}

.keywords {
  display: flex;
  flex-wrap: wrap;
  gap: var(--space-2);
}

.block {
  padding: var(--space-3);
  border: 1px solid var(--c-border);
  border-radius: var(--radius);
  background: var(--c-surface);
  line-height: 1.55;
}

.block--shadow {
  border-left: 3px solid var(--c-enemy);
  background: var(--c-surface-2);
}

.block--back {
  border-style: dashed;
}

.block__title {
  margin-bottom: var(--space-2);
  font-size: var(--text-xs);
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--c-text-faint);
}

.back-stat {
  margin-top: var(--space-2);
  font-size: var(--text-sm);
  color: var(--c-text-muted);
}

.flavor {
  color: var(--c-text-muted);
  font-style: italic;
  font-size: var(--text-sm);
  text-wrap: pretty;
}

.facts {
  display: grid;
  grid-template-columns: auto 1fr;
  gap: var(--space-1) var(--space-4);
  padding-top: var(--space-3);
  border-top: 1px solid var(--c-border);
  font-size: var(--text-sm);
}

.facts dt {
  color: var(--c-text-faint);
}

.links {
  margin-top: var(--space-1);
}

@container (width < 34rem) {
  .detail {
    grid-template-columns: minmax(0, 1fr);
  }

  .detail__art {
    position: static;
    max-width: 20rem;
    margin-inline: auto;
    width: 100%;
  }
}
</style>
