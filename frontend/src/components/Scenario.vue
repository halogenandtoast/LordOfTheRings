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
</script>

<template>
  <section class="band scenario">
    <span class="band-label">{{ $t('game.bands.scenario') }}</span>
    <DiscardPile :cards="scenario.discard" back="encounter" data-pile="discard-encounter" />
    <div class="deck">
      <CardBack :count="scenario.encounterDeck" back="encounter" data-pile="deck-encounter" />
      <div v-if="debugging && playerId" class="deck-debug">
        <button type="button" :disabled="scenario.encounterDeck === 0" @click="send({ tag: 'RevealEncounterCard' })">{{ $t('game.debug.reveal') }}</button>
        <button type="button" :disabled="scenario.encounterDeck === 0" @click="send({ tag: 'DebugSearchEncounterDeck', contents: playerId })">{{ $t('game.debug.search') }}</button>
      </div>
    </div>
    <GameCard
      v-if="scenario.quest"
      :card="scenario.quest"
      rotated
      :choices="choices"
      :flipping="isFlipping(scenario.quest)"
      @choose="emit('choose', $event)"
    />
    <GameCard
      v-if="scenario.activeLocation"
      :card="scenario.activeLocation"
      :choices="choices"
      :flipping="isFlipping(scenario.activeLocation)"
      @choose="emit('choose', $event)"
    />
    <!-- where the travelled-to location goes, kept when there is none -->
    <div v-else class="location-placeholder">{{ $t('game.noActiveLocation') }}</div>
  </section>
</template>

<style scoped>
.scenario { flex: 0 0 auto; }
.deck { display: flex; flex-direction: column; gap: 4px; }
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
