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
  </section>
</template>

<style scoped>
.scenario { flex: 0 0 auto; }
.deck { display: flex; flex-direction: column; gap: 4px; }
</style>
