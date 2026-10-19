<script setup lang="ts">
import CardBack from './CardBack.vue'
import DiscardPile from './DiscardPile.vue'
import GameCard from './GameCard.vue'
import QuestionZone from './QuestionZone.vue'
import type { ChoiceView, PlayerView, QuestionView } from '../types'

const { player, choices, flipping } = defineProps<{
  player: PlayerView
  choices: ChoiceView[]
  flipping: ReadonlySet<string>
  question: QuestionView | null
}>()
const emit = defineEmits<{ choose: [index: number] }>()

</script>

<template>
  <section class="band player" :class="{ eliminated: player.eliminated }">
    <div class="piles">
      <DiscardPile :cards="player.discard" back="player" :data-pile="`discard-${player.playerId}`" />
      <CardBack :count="player.deckSize" back="player" :data-pile="`deck-${player.playerId}`" />
    </div>

    <div class="hand">
      <GameCard
        v-for="card in player.hand"
        :key="card.id"
        :card="card"
        :choices="choices"
        :flipping="flipping.has(card.id)"
        draggable
        @choose="emit('choose', $event)"
      />
    </div>

    <QuestionZone :question="question" @choose="emit('choose', $event)" />
  </section>
</template>

<style scoped>
.player { flex: 0 0 auto; align-items: flex-end; margin-top: auto; }
.player.eliminated { opacity: 0.45; }
.hand, .piles { display: flex; gap: var(--gap); }
.hand { flex: 1; flex-wrap: wrap; justify-content: flex-start; }


</style>
