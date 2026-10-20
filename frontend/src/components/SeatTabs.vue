<script setup lang="ts">
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
      <span v-if="player.playerId === asked" class="asked" :title="$t('game.seats.asked')" />
      <span class="name">{{ player.name }}</span>
      <span v-if="player.playerId === mine" class="you">{{ $t('game.lobby.you') }}</span>
      <span v-if="player.firstPlayer" class="first" :title="$t('game.seats.firstPlayer')">①</span>
      <span class="stat" :title="$t('game.threat', { value: player.threat })">
        <img src="/images/threat.png" alt="" />{{ player.threat }}
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
  margin-top: -1px;
  padding: 8px 14px;
  border: 1px solid transparent;
  border-top: 0;
  border-radius: 0 0 6px 6px;
  background: none;
  color: var(--text-muted);
  font: inherit;
  font-size: 0.85rem;
  white-space: nowrap;
  cursor: pointer;
}
.tab:hover { color: var(--text); background: var(--surface-1); }
/* the board's tab hangs from it, joined to it */
.tab.active {
  border-color: var(--line-strong);
  background: var(--surface-2);
  color: var(--text);
  box-shadow: inset 0 3px 0 var(--accent);
}
.tab.eliminated { opacity: 0.45; }
.name { font-weight: 700; }
.you { font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.08em; color: var(--accent-bright); }
.stat { display: inline-flex; align-items: center; gap: 3px; font-size: 0.75rem; color: var(--text-muted); }
.stat img { width: 14px; height: 14px; object-fit: contain; }
.asked {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  background: var(--accent-bright);
  animation: pulse 1.4s ease-in-out infinite;
}
@keyframes pulse { 50% { opacity: 0.3; } }
@media (prefers-reduced-motion: reduce) { .asked { animation: none; } }
</style>
