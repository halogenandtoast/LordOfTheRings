<script lang="ts" setup>
import { computed, ref } from 'vue'
import type { GameSummary } from '../server'

const props = defineProps<{ game: GameSummary; deleteGame?: () => void; spectate?: boolean }>()

const deleting = ref(false)
const phase = computed(() => `game.phases.${props.game.phase.replace(/Phase$/, '').toLowerCase()}`)
</script>

<template>
  <div class="game" :class="{ 'finished-game': game.outcome, won: game.outcome === 'Won', lost: game.outcome === 'Lost' }">
    <div class="game-details">
      <div class="game-title">
        <div class="main-details">
          <div class="scenario-icon-container">
            <img class="scenario-icon" src="/images/cards/01119.jpg" alt="" />
          </div>
          <RouterLink class="title" :to="spectate ? `/games/${game.id}/spectate` : `/games/${game.id}`">{{ game.name }}</RouterLink>
          <div v-if="game.easyMode" class="badge easy">{{ $t('home.easyMode') }}</div>
          <div v-if="game.outcome" class="badge outcome" :class="game.outcome === 'Won' ? 'won' : 'lost'">
            {{ $t(game.outcome === 'Won' ? 'home.won' : 'home.lost') }}
          </div>
        </div>
        <div class="extra-details">
          <div v-if="deleteGame" class="game-delete">
            <a href="#delete" :aria-label="$t('home.deleteGame')" @click.prevent="deleting = true">
              <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                <path stroke-linecap="round" stroke-linejoin="round" d="m14.74 9-.346 9m-4.788 0L9.26 9m9.968-3.21c.342.052.682.107 1.022.166m-1.022-.165L18.16 19.673a2.25 2.25 0 0 1-2.244 2.077H8.084a2.25 2.25 0 0 1-2.244-2.077L4.772 5.79m14.456 0a48.108 48.108 0 0 0-3.478-.397m-12 .562c.34-.059.68-.114 1.022-.165m0 0a48.11 48.11 0 0 1 3.478-.397m7.5 0v-.916c0-1.18-.91-2.164-2.09-2.201a51.964 51.964 0 0 0-3.32 0c-1.18.037-2.09 1.022-2.09 2.201v.916m7.5 0a48.667 48.667 0 0 0-7.5 0" />
              </svg>
            </a>
          </div>
        </div>
      </div>
      <div class="game-progress">
        <!-- otherwise named beside their heroes -->
        <span v-if="!game.seats?.length">{{ game.playerName }}</span>
        <span v-if="game.waiting">{{ $t('home.waitingForPlayers') }}</span>
        <template v-else>
          <span>{{ $t('game.round', { round: game.round }) }} &middot; {{ $t(phase) }}</span>
          <span>{{ $t('home.threat', { threat: game.threat }) }}</span>
        </template>
      </div>
      <!-- each player's heroes, as Arkham's rows show their investigators -->
      <ul v-if="game.seats?.length" class="seats">
        <li v-for="(seat, n) in game.seats" :key="n" class="seat" :class="{ eliminated: seat.eliminated }">
          <span class="heroes">
            <span
              v-for="hero in seat.heroes"
              :key="hero.code"
              class="hero"
              :class="hero.sphere?.toLowerCase()"
              :style="{ backgroundImage: `url('/images/cards/${hero.code}.jpg')` }"
            />
          </span>
          <span class="seat-name">{{ seat.name }}</span>
          <span class="seat-threat" :title="$t('game.threat', { value: seat.threat })">
            {{ seat.threat }}<span class="icon" :style="{ '--icon': `url('/images/icons/game/threat.svg')` }" />
          </span>
        </li>
      </ul>
    </div>
    <div v-if="deleting" class="confirm-delete">
      <span>{{ $t('home.confirmDelete') }}</span>
      <button type="button" class="confirm" @click="deleteGame?.()">{{ $t('common.delete') }}</button>
      <button type="button" @click="deleting = false">{{ $t('common.cancel') }}</button>
    </div>
  </div>
</template>

<style scoped>
.game {
  background-color: var(--box-background);
  border: 1px solid var(--box-border);
  border-left: 4px solid var(--accent);
  border-radius: 5px;
  margin-bottom: 10px;
  padding: 10px 14px;
  color: var(--text);
}
.game.finished-game { opacity: 0.85; }
.game.won { border-left-color: var(--success); }
.game.lost { border-left-color: var(--danger); }

.game-title {
  display: flex;
  align-items: center;
  gap: 10px;
}

.main-details {
  display: flex;
  align-items: center;
  gap: 10px;
  flex: 1;
  min-width: 0;
}

.scenario-icon-container {
  width: 64px;
  aspect-ratio: 1 / 0.715;
  border-radius: 3px;
  overflow: hidden;
  flex: none;
  box-shadow: 0 1px 4px var(--shadow);
}
.scenario-icon { width: 100%; height: 100%; object-fit: cover; }

.title {
  color: var(--title);
  font-size: 1.4em;
  font-weight: 700;
  text-decoration: none;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
.title:hover { color: var(--accent-bright); text-decoration: underline; }

.badge {
  flex: none;
  border-radius: 3px;
  font-size: 0.7em;
  font-weight: 700;
  padding: 2px 8px;
  text-transform: uppercase;
  letter-spacing: 0.06em;
}
.badge.easy { border: 1px solid var(--box-border); color: var(--text-muted); padding: 1px 7px; }
.outcome.won { background: var(--success); color: var(--button-primary-text); }
.outcome.lost { background: var(--danger); color: var(--text); }

.extra-details {
  display: flex;
  align-items: center;
  gap: 12px;
}

.game-delete a { color: var(--text-muted); display: flex; }
.game-delete a:hover { color: var(--danger); }
.game-delete svg { width: 18px; }

.game-progress {
  display: flex;
  gap: 16px;
  margin-top: 6px;
  color: var(--text-muted);
  font-size: 0.8em;
  text-transform: uppercase;
  letter-spacing: 0.05em;
}

.seats {
  display: flex;
  flex-wrap: wrap;
  gap: 8px 18px;
  margin: 10px 0 0;
  padding: 8px 10px;
  list-style: none;
  background: rgb(229 211 173 / 0.03);
  border-radius: 4px;
}
.seat { display: flex; align-items: center; gap: 8px; }
.seat.eliminated { opacity: 0.45; }
.heroes { display: flex; gap: 4px; }
/* the art off the hero card, framed in its sphere's colour */
.hero {
  width: 42px;
  height: 42px;
  border: 2px solid var(--neutral);
  border-radius: 5px;
  background-size: 230%;
  background-position: 50% 28%;
  box-shadow: 1px 1px 5px var(--shadow);
}
.hero.leadership { border-color: var(--leadership); }
.hero.lore { border-color: var(--lore); }
.hero.spirit { border-color: var(--spirit); }
.hero.tactics { border-color: var(--tactics); }
.seat-name { color: var(--text); font-size: 0.85em; font-weight: 600; }
.seat-threat { display: flex; align-items: center; gap: 2px; color: var(--threat-red); font-size: 0.85em; font-weight: 700; }
.seat-threat .icon {
  display: inline-block;
  width: 1em;
  height: 1em;
  background: currentColor;
  mask: var(--icon) center / contain no-repeat;
}

.confirm-delete {
  display: flex;
  align-items: center;
  gap: 8px;
  margin-top: 10px;
  font-size: 0.9em;
}
.confirm-delete span { flex: 1; }
.confirm-delete button {
  background: transparent;
  border: 1px solid var(--box-border);
  border-radius: 3px;
  color: var(--text);
  cursor: pointer;
  font: inherit;
  font-size: 0.85em;
  font-weight: 700;
  padding: 5px 10px;
  text-transform: uppercase;
}
.confirm-delete button.confirm { background: var(--danger); border-color: var(--danger); }
</style>
