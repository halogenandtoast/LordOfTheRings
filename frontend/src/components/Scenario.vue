<script setup lang="ts">
import CardBack from './CardBack.vue'
import DiscardPile from './DiscardPile.vue'
import GameCard from './GameCard.vue'
import type { CardView, ChoiceView, ScenarioView } from '../types'
import { useDebug } from '../composables/useDebug'

const { scenario, choices, flipping } = defineProps<{
  scenario: ScenarioView
  choices: ChoiceView[]
  flipping: ReadonlySet<string>
  /** Who searches the encounter deck in debug mode. */
  playerId: string | null
}>()
const emit = defineEmits<{ choose: [index: number] }>()
const { active: debugging, send } = useDebug()

const isFlipping = (card: CardView) => flipping.has(card.id)

// the heading's four settings, on-course first (Dream-chaser rules)
const settings = ['sunny', 'cloudy', 'rainy', 'stormy'].map((key) => ({
  key,
  icon: { '--icon': `url('/images/icons/game/heading-${key}.png')` },
}))
</script>

<template>
  <section class="band scenario">
    <span class="band-label">{{ $t('game.bands.scenario') }}</span>
    <DiscardPile :cards="scenario.discard" back="encounter" :choices="choices" @choose="emit('choose', $event)" data-pile="discard-encounter" />
    <div class="deck">
      <CardBack :count="scenario.encounterDeck" back="encounter" data-pile="deck-encounter" />
      <div v-if="debugging && playerId" class="deck-debug">
        <button type="button" :disabled="scenario.encounterDeck === 0" @click="send({ tag: 'RevealEncounterCard' })">{{ $t('game.debug.reveal') }}</button>
        <button type="button" :disabled="scenario.encounterDeck === 0" @click="send({ tag: 'DebugSearchEncounterDeck', contents: playerId })">{{ $t('game.debug.search') }}</button>
      </div>
    </div>
    <!-- the Corsair deck beside the encounter deck, with its own discard pile -->
    <template v-if="scenario.corsairDeck !== null">
      <DiscardPile :cards="scenario.corsairDiscard" back="encounter" :choices="choices" @choose="emit('choose', $event)" data-pile="discard-corsair" />
      <CardBack :count="scenario.corsairDeck" back="encounter" :title="$t('log.corsairDeck', { count: scenario.corsairDeck })" data-pile="deck-corsair" />
    </template>
    <div v-if="scenario.heading" class="heading" :title="$t(`log.heading.${scenario.heading}`)">
      <span class="heading-label">{{ $t('game.heading') }}</span>
      <ol class="heading-track">
        <li v-for="s in settings" :key="s.key" :class="{ current: s.key === scenario.heading }"><span class="icon" :style="s.icon" /></li>
      </ol>
    </div>
    <!-- in campaign mode, the campaign card next to the quest deck -->
    <GameCard v-if="scenario.campaignCard" :card="scenario.campaignCard" :choices="choices" @choose="emit('choose', $event)" />
    <GameCard
      v-if="scenario.quest"
      :card="scenario.quest"
      :attachments="scenario.questAttachments"
      rotated
      :choices="choices"
      :flipping="isFlipping(scenario.quest)"
      @choose="emit('choose', $event)"
    />
    <!-- the Island Map in its 3x5 grid, in place of the active location's slot -->
    <div v-if="scenario.islandMap.length" class="island-map">
      <div
        v-for="cell in scenario.islandMap"
        :key="cell.card.id"
        class="island-cell"
        :class="{ active: cell.active, staged: cell.staged }"
        :style="{ gridColumn: cell.column + 1, gridRow: cell.row + 1 }"
      >
        <GameCard :card="cell.card" :attachments="cell.attachments" :choices="choices" :flipping="isFlipping(cell.card)" @choose="emit('choose', $event)" />
      </div>
    </div>
    <GameCard
      v-else-if="scenario.activeLocation"
      :card="scenario.activeLocation"
      :attachments="scenario.activeLocationAttachments"
      :choices="choices"
      :flipping="isFlipping(scenario.activeLocation)"
      @choose="emit('choose', $event)"
    />
    <!-- where the travelled-to location goes, kept when there is none -->
    <div v-else class="location-placeholder">{{ $t('game.noActiveLocation') }}</div>
    <GameCard
      v-for="other in scenario.otherActiveLocations"
      :key="other.card.id"
      :card="other.card"
      :attachments="other.attachments"
      :choices="choices"
      :flipping="isFlipping(other.card)"
      @choose="emit('choose', $event)"
    />
    <!-- the Stormcaller's area: its own quest stage, and the cards not yet in the staging area -->
    <div v-if="scenario.stormcallerStage" class="stormcaller-area">
      <span class="heading-label">{{ $t('log.stormcallerArea') }}</span>
      <div class="stormcaller-cards">
        <GameCard :card="scenario.stormcallerStage" rotated :choices="choices" @choose="emit('choose', $event)" />
        <GameCard
          v-for="c in scenario.stormcallerArea"
          :key="c.id"
          :card="c"
          :choices="choices"
          :flipping="isFlipping(c)"
          @choose="emit('choose', $event)"
        />
      </div>
    </div>
  </section>
</template>

<style scoped>
.scenario { flex: 0 0 auto; }
.deck { display: flex; flex-direction: column; gap: 4px; }
.heading { display: flex; flex-direction: column; align-items: center; gap: 4px; font-size: 0.6rem; letter-spacing: 0.08em; text-transform: uppercase; color: var(--text-muted); }
.heading-track { display: flex; flex-direction: column; gap: 2px; margin: 0; padding: 0; list-style: none; }
.heading-track li { width: 1.6rem; height: 1.6rem; display: grid; place-items: center; border: 1px solid var(--line-strong); border-radius: 4px; font-size: 0.9rem; opacity: 0.4; }
.heading-track li.current { opacity: 1; border-color: currentColor; color: var(--text); }
.stormcaller-area { display: flex; flex-direction: column; gap: 4px; padding-left: 8px; border-left: 1px dashed var(--line-strong); font-size: 0.6rem; letter-spacing: 0.08em; text-transform: uppercase; color: var(--text-muted); }
.stormcaller-cards { display: flex; gap: 6px; align-items: flex-start; }
.island-map { display: grid; grid-template-columns: repeat(5, auto); grid-template-rows: repeat(3, auto); gap: 6px; }
.island-cell { border: 2px solid transparent; border-radius: 8px; padding: 2px; opacity: 0.75; }
.island-cell.staged { opacity: 1; border-color: var(--line-strong); }
.island-cell.active { opacity: 1; border-color: var(--text); }
.location-placeholder {
  display: flex;
  align-items: center;
  justify-content: center;
  width: var(--card-width);
  aspect-ratio: var(--card-aspect);
  padding: 6px;
  border: 1px dashed var(--line-strong);
  border-radius: 6px;
  color: var(--text-muted);
  font-size: 0.6rem;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  text-align: center;
}
</style>
