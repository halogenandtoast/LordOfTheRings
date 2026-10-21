<script setup lang="ts">
import { computed, onMounted, ref } from 'vue'
import DeckPicker from './DeckPicker.vue'
import { fetchDecks } from '../browser/lib/api'
import type { DeckSummary } from '../browser/types/deck'
import { currentUser } from '../auth'
import { toDeckChoice, type DeckKey } from '../deckChoice'
import type { DeckChoice } from '../api'
import type { Table } from '../server'

// A game before it is dealt: who has sat down, the link to send friends, and a seat to
// take with a deck. The owner may take several. Taking the last seat deals the game.
const { table } = defineProps<{ table: Table }>()
const emit = defineEmits<{ join: [deck: DeckChoice]; leave: []; start: [] }>()

const seats = computed(() =>
  Array.from({ length: table.seats }, (_, position) => table.seated.find((s) => s.position === position) ?? null),
)
const mine = computed(() => table.seated.filter((s) => s.userId === currentUser.value?.id).length)
const full = computed(() => table.seated.length >= table.seats)
const owner = computed(() => table.owner === currentUser.value?.id)

const link = window.location.href
const copied = ref(false)
const copy = async () => {
  await navigator.clipboard.writeText(link)
  copied.value = true
}

const decks = ref<DeckSummary[]>([])
const deck = ref<DeckKey>('starter:leadership')
onMounted(async () => {
  try {
    decks.value = await fetchDecks()
  } catch {
    // the starter decks are enough to play with
  }
})
</script>

<template>
  <section class="lobby">
    <h2>{{ $t('game.lobby.waiting') }}</h2>

    <ol class="seats">
      <li v-for="(seat, position) in seats" :key="position" class="seat" :class="{ open: !seat, you: seat?.userId === currentUser?.id }">
        <span class="number">{{ position + 1 }}</span>
        <span v-if="seat" class="name">{{ seat.username }}<template v-if="seat.userId === currentUser?.id"> · {{ $t('game.lobby.you') }}</template></span>
        <span v-else class="name">{{ $t('newGame.openSeat') }}</span>
      </li>
    </ol>

    <div v-if="table.seats > 1" class="invite">
      <span>{{ $t('game.lobby.invite') }}</span>
      <div class="link">
        <input :value="link" type="text" readonly @focus="($event.target as HTMLInputElement).select()" />
        <button type="button" @click="copy">{{ copied ? $t('game.lobby.copied') : $t('game.lobby.copy') }}</button>
      </div>
    </div>

    <template v-if="!full && (owner || mine === 0)">
      <h3>{{ $t('game.lobby.chooseDeck') }}</h3>
      <DeckPicker v-model="deck" :decks="decks" />
      <button type="button" class="join" @click="emit('join', toDeckChoice(deck))">{{ $t(mine > 0 ? 'game.lobby.takeAnother' : 'game.lobby.join') }}</button>
    </template>
    <template v-if="mine > 0">
      <p class="hint">{{ $t('game.lobby.dealtWhenFull') }}</p>
      <div class="actions">
        <button v-if="owner" type="button" class="join" @click="emit('start')">{{ $t('game.lobby.startNow', table.seated.length) }}</button>
        <button type="button" class="secondary" @click="emit('leave')">{{ $t('game.lobby.leave', mine) }}</button>
      </div>
    </template>
  </section>
</template>

<style scoped>
.lobby {
  display: grid;
  gap: 16px;
  width: min(720px, 100%);
  margin: 24px auto;
  padding: 20px;
  border: 1px solid var(--box-border);
  border-radius: 6px;
  background: var(--box-background);
}
h2, h3 { margin: 0; color: var(--title); }
h2 { text-transform: uppercase; letter-spacing: 0.04em; }
h3 { font-size: 0.8rem; letter-spacing: 0.1em; text-transform: uppercase; }
.seats { display: grid; grid-template-columns: repeat(auto-fill, minmax(150px, 1fr)); gap: 10px; margin: 0; padding: 0; list-style: none; }
.seat {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 10px 12px;
  border: 2px solid var(--line);
  border-radius: 6px;
  background: var(--input-background);
}
.seat.open { border-style: dashed; color: var(--text-muted); background: none; }
.seat.you { border-color: var(--accent); }
.number {
  display: grid;
  place-items: center;
  width: 24px;
  height: 24px;
  border-radius: 50%;
  background: var(--surface-4);
  font-size: 0.8rem;
  font-weight: 700;
}
.name { font-weight: 700; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.invite { display: grid; gap: 6px; font-size: 0.85rem; color: var(--text-muted); }
.link { display: flex; gap: 8px; }
.link input {
  flex: 1;
  min-width: 0;
  padding: 10px;
  border: 1px solid var(--box-border);
  border-radius: 3px;
  background: var(--input-background);
  color: var(--text);
  font: inherit;
}
.link button, .join {
  padding: 10px 16px;
  border: 0;
  border-radius: 3px;
  background: var(--button-primary);
  color: var(--button-primary-text);
  font: inherit;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  cursor: pointer;
}
.link button:hover, .join:hover { background: var(--button-primary-hover); }
.join { padding: 15px; }
.actions { display: flex; gap: 10px; }
.actions .join { flex: 1; }
.secondary {
  padding: 15px;
  border: 0;
  border-radius: 3px;
  background: var(--button-secondary);
  color: var(--text);
  font: inherit;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  cursor: pointer;
}
.secondary:hover { background: var(--button-secondary-hover); }
.hint { margin: 0; color: var(--text-muted); }
</style>
