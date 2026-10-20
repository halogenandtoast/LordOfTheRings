<script setup lang="ts">
import { ref, useTemplateRef } from 'vue'
import { onClickOutside } from '@vueuse/core'
import CardBack from './CardBack.vue'
import DiscardPile from './DiscardPile.vue'
import GameCard from './GameCard.vue'
import QuestionZone from './QuestionZone.vue'
import ThreatDial from './ThreatDial.vue'
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

// Pinned to the foot of a small screen, the hand peeks out until tapped, as in Arkham.
const handOpen = ref(false)
onClickOutside(useTemplateRef('hand'), () => (handOpen.value = false))

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

    <div ref="hand" class="hand" :class="{ open: handOpen }" :style="{ '--count': player.hand.length }" @click="handOpen = true">
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

    <div class="threat">
      <ThreatDial :threat="player.threat" />
      <div v-if="debugging" class="debug-threat">
        <button type="button" :aria-label="$t('game.debug.lowerThreat')" :disabled="player.threat === 0" @click="send({ tag: 'LowerThreat', contents: [player.playerId, 1] })">−</button>
        <button type="button" :aria-label="$t('game.debug.raiseThreat')" @click="send({ tag: 'RaiseThreat', contents: [player.playerId, 1] })">+</button>
      </div>
    </div>

    <QuestionZone :question="question" @choose="emit('choose', $event)" />
  </section>
</template>

<style scoped>
.player { flex: 0 0 auto; flex-wrap: nowrap; align-items: flex-end; margin-top: auto; }
.player.eliminated { opacity: 0.45; }
.hand, .piles { display: flex; gap: var(--gap); }
.deck { display: flex; flex-direction: column; gap: 4px; }
.threat { display: flex; flex-direction: column; align-items: center; align-self: center; gap: var(--gap); }
.debug-threat { display: flex; gap: var(--gap); }
.debug-threat button { width: 28px; height: 22px; padding: 0; border: 1px solid var(--line); border-radius: 4px; background: none; color: inherit; cursor: pointer; }
.debug-threat button:disabled { opacity: 0.35; cursor: default; }
/* One row that fans out: cards sit apart while they fit, then overlap to fit the width
   (a margin's % is of the hand's width). Hovering a card lifts it over its neighbours. */
.hand { flex: 1 1 0; min-width: calc(var(--card-width) * 2); gap: 0; }
.hand > * + * {
  margin-left: max(
    calc(var(--card-width) * -0.8),
    min(var(--gap), calc((100% - var(--count) * var(--card-width)) / (var(--count) - 1)))
  );
}
.hand > * { transition: margin 0.2s ease, translate 0.15s ease; }
.hand > :hover { z-index: 10; translate: 0 -8%; }
/* level with the deck and discard, not their count pills: a pill's height and the stack's gap */
.player .hand { margin-bottom: calc(0.75rem + 8px + 5px); }
/* docked under the board instead (see Game.vue) */
@media (width < 1100px), (height < 500px) {
  .player .question { display: none; }
}
/* not enough room on the board: pinned to the foot of the screen (Game.vue leaves room) */
@media (width < 700px), (height < 500px) {
  .player .hand {
    position: fixed;
    inset: auto 0 0 0;
    z-index: 55;
    margin: 0;
    padding: 8px var(--gap) 0;
    background: var(--surface-1);
    border-top: 1px solid var(--line-strong);
    box-shadow: 0 -4px 16px var(--shadow);
    translate: 0 calc(100% - var(--hand-peek));
    transition: translate 0.3s ease;
  }
  .player .hand.open { translate: 0 0; }
  /* the first tap opens it rather than playing a card */
  .player .hand:not(.open) > * { pointer-events: none; }
}


</style>
