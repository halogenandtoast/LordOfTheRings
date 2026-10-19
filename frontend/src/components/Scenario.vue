<script setup lang="ts">
import CardBack from './CardBack.vue'
import DiscardPile from './DiscardPile.vue'
import GameCard from './GameCard.vue'
import type { CardView, ChoiceView, ScenarioView } from '../types'

const { scenario, choices, flipping } = defineProps<{
  scenario: ScenarioView
  choices: ChoiceView[]
  flipping: ReadonlySet<string>
}>()
const emit = defineEmits<{ choose: [index: number] }>()

const isFlipping = (card: CardView) => flipping.has(card.id)
</script>

<template>
  <section class="band scenario">
    <DiscardPile :cards="scenario.discard" back="encounter" data-pile="discard-encounter" />
    <CardBack :count="scenario.encounterDeck" back="encounter" data-pile="deck-encounter" />
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
</style>
