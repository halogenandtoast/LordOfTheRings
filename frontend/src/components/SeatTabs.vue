<script setup lang="ts">
import { LoaderCircle } from 'lucide-vue-next'
import type { PlayerView } from '../types'

// A tab per seat along the foot of the board, for looking at another seat's board.
defineProps<{
  players: PlayerView[]
  viewing: string | null
  /** The seat being asked something. */
  asked: string | null
  /** Your own seat at a game with friends. */
  mine: string | null
}>()
const emit = defineEmits<{ view: [playerId: string] }>()

const threatIcon = { '--icon': "url('/images/icons/game/threat.svg')" }
</script>

<template>
  <nav class="seat-tabs" role="tablist" :aria-label="$t('game.seats.label')">
    <button
      v-for="player in players"
      :key="player.playerId"
      type="button"
      role="tab"
      class="tab"
      :class="{ active: player.playerId === viewing, eliminated: player.eliminated }"
      :aria-selected="player.playerId === viewing"
      @click="emit('view', player.playerId)"
    >
      <LoaderCircle v-if="player.playerId === asked" class="asked" :size="14" :aria-label="$t('game.seats.asked')" />
      <span class="name">{{ player.name }}</span>
      <span v-if="player.playerId === mine" class="you">{{ $t('game.lobby.you') }}</span>
      <span v-if="player.firstPlayer" class="first" :title="$t('game.seats.firstPlayer')">①</span>
      <span class="stat" :title="$t('game.threat', { value: player.threat })">
        <span class="icon threat" :style="threatIcon" />{{ player.threat }}
      </span>
      <span class="stat">{{ $t('game.seats.hand', { count: player.handSize }) }}</span>
    </button>
  </nav>
</template>

<style scoped>
.seat-tabs {
  position: sticky;
  bottom: 0;
  z-index: 5;
  display: flex;
  gap: 4px;
  padding: 0 var(--gap);
  border-top: 1px solid var(--line-strong);
  background: var(--surface-0);
  overflow-x: auto;
}
.tab {
  position: relative;
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 8px 14px;
  border: 0;
  background: none;
  color: var(--text-muted);
  font: inherit;
  font-size: 0.85rem;
  white-space: nowrap;
  cursor: pointer;
}
.tab:hover { color: var(--text); background: var(--surface-1); }
.tab.active {
  background: var(--surface-2);
  color: var(--text);
}
.tab.eliminated { opacity: 0.45; }
.name { font-weight: 700; }
.you { font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.08em; color: var(--accent-bright); }
.stat { display: inline-flex; align-items: center; gap: 3px; font-size: 0.75rem; color: var(--text-muted); }
.stat .threat { font-size: 14px; }
.asked { color: var(--accent-bright); animation: spin 1s linear infinite; }
@keyframes spin { to { rotate: 360deg; } }
@media (prefers-reduced-motion: reduce) { .asked { animation: none; } }
</style>
