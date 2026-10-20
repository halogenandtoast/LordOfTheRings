<script setup lang="ts">
import CardBack from './CardBack.vue'
import DiscardPile from './DiscardPile.vue'
import GameCard from './GameCard.vue'
import QuestionZone from './QuestionZone.vue'
import type { ChoiceView, PlayerView, QuestionView } from '../types'
import { useDebug } from '../composables/useDebug'

const { player, choices, flipping } = defineProps<{
  player: PlayerView
  choices: ChoiceView[]
  flipping: ReadonlySet<string>
  question: QuestionView | null
}>()
const emit = defineEmits<{ choose: [index: number] }>()
const { active: debugging, send } = useDebug()

</script>

<template>
  <section class="band player" :class="{ eliminated: player.eliminated }">
    <div class="piles">
      <DiscardPile :cards="player.discard" back="player" :data-pile="`discard-${player.playerId}`" />
      <div class="deck">
        <CardBack :count="player.deckSize" back="player" :data-pile="`deck-${player.playerId}`" />
        <div v-if="debugging" class="deck-debug">
          <button type="button" :disabled="player.deckSize === 0" @click="send({ tag: 'DrawCards', contents: { player: player.playerId, count: 1 } })">{{ $t('game.debug.draw') }}</button>
          <button type="button" :disabled="player.deckSize === 0" @click="send({ tag: 'DebugSearchDeck', contents: player.playerId })">{{ $t('game.debug.search') }}</button>
        </div>
      </div>
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
.deck { display: flex; flex-direction: column; gap: 4px; }
.hand { flex: 1; flex-wrap: wrap; justify-content: flex-start; }
/* level with the deck and discard, not their count pills: a pill's height and the stack's gap */
.player .hand { margin-bottom: calc(0.75rem + 8px + 5px); }
/* docked under the board instead (see Game.vue) */
@media (width < 1100px), (height < 500px) {
  .player .question { display: none; }
}


</style>
