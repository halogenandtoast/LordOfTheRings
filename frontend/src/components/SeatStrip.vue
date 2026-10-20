<script setup lang="ts">
import CardBand from './CardBand.vue'
import type { ChoiceView, PlayerView } from '../types'

// Another seat at the table, small: its characters, what it has engaged and its counts.
// Clicking its name looks at its board instead.
defineProps<{
  player: PlayerView
  choices: ChoiceView[]
  flipping: ReadonlySet<string>
  /** This seat is being asked something. */
  asked: boolean
}>()
const emit = defineEmits<{ choose: [index: number]; view: [] }>()
</script>

<template>
  <section class="seat" :class="{ eliminated: player.eliminated, asked }">
    <button type="button" class="seat-name" :title="$t('game.seats.view', { name: player.name })" @click="emit('view')">
      <span class="name">{{ player.name }}</span>
      <span v-if="player.firstPlayer" class="first" :title="$t('game.seats.firstPlayer')">①</span>
      <span class="count">{{ $t('game.threat', { value: player.threat }) }}</span>
      <span class="count">{{ $t('game.seats.cards', { hand: player.handSize, deck: player.deckSize }) }}</span>
    </button>
    <div class="seat-cards">
      <CardBand :cards="player.inPlay" :choices="choices" :flipping="flipping" align="bottom" @choose="emit('choose', $event)" />
      <CardBand
        :label="$t('game.bands.threatArea')"
        :cards="player.engaged"
        :choices="choices"
        :flipping="flipping"
        align="bottom"
        @choose="emit('choose', $event)"
      />
    </div>
  </section>
</template>

<style scoped>
.seat {
  /* a smaller table, so several fit beside your own */
  --card-width: clamp(40px, min(6vw, 8vh), 64px);
  --attachment-peek: 15px;
  display: flex;
  flex-direction: column;
  gap: 4px;
  padding: 6px;
  border: 1px solid var(--line);
  border-radius: 6px;
}
.seat.asked { border-color: var(--accent); }
.seat.eliminated { opacity: 0.45; }
.seat-name {
  display: flex;
  align-items: baseline;
  gap: 8px;
  padding: 0;
  border: 0;
  background: none;
  color: inherit;
  font: inherit;
  text-align: left;
  cursor: pointer;
}
.seat-name:hover .name { text-decoration: underline; }
.name { font-weight: 700; }
.count { font-size: 0.75rem; color: var(--text-muted); }
.seat-cards { display: flex; gap: var(--gap); align-items: flex-end; }
</style>
