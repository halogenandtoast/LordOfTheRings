<script lang="ts" setup>
import { computed, useTemplateRef } from 'vue'
import ConfirmDelete from './ConfirmDelete.vue'
import TrashIcon from './TrashIcon.vue'
import type { GameSummary } from '../server'
import { slug } from '../campaigns'

const props = defineProps<{ game: GameSummary; deleteGame?: () => void; spectate?: boolean }>()

const confirm = useTemplateRef<InstanceType<typeof ConfirmDelete>>('confirm')
// the scenario's encounter set icon, as the new game page shows it
const icon = computed(() => `url('/images/icons/scenarios/${slug(props.game.scenario)}.svg')`)
const phase = computed(() => `game.phases.${props.game.phase.replace(/Phase$/, '').toLowerCase()}`)
</script>

<template>
  <div class="game" :class="{ 'finished-game': game.outcome, won: game.outcome === 'Won', lost: game.outcome === 'Lost' }">
    <div class="game-details">
      <div class="game-title">
        <div class="main-details">
          <span class="scenario-icon" role="img" :aria-label="game.scenario" :title="game.scenario" :style="{ '--icon': icon }" />
          <RouterLink class="title" :to="spectate ? `/games/${game.id}/spectate` : `/games/${game.id}`">{{ game.name }}</RouterLink>
          <div v-if="game.easyMode" class="badge easy">{{ $t('home.easyMode') }}</div>
          <div v-if="game.outcome" class="badge outcome" :class="game.outcome === 'Won' ? 'won' : 'lost'">
            {{ $t(game.outcome === 'Won' ? 'home.won' : 'home.lost') }}
          </div>
          <div v-if="game.outcome" class="badge victory" :title="$t('home.victoryPoints', { value: game.victoryPoints })">
            {{ $t('home.vp', { value: game.victoryPoints }) }}
          </div>
        </div>
        <div class="extra-details">
          <div v-if="deleteGame" class="game-delete">
            <a href="#delete" :aria-label="$t('home.deleteGame')" @click.prevent="confirm?.open()">
              <TrashIcon />
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
              :style="{ backgroundImage: `url('/images/cards/${hero.code}.avif')` }"
            />
          </span>
          <span class="seat-caption">
            <span class="seat-name">{{ seat.name }}</span>
            <span class="seat-threat" :title="$t('game.threat', { value: seat.threat })">
              {{ seat.threat }}<span class="icon" :style="{ '--icon': `url('/images/icons/game/threat.svg')` }" />
            </span>
          </span>
        </li>
      </ul>
    </div>
    <ConfirmDelete v-if="deleteGame" ref="confirm" :title="$t('home.confirmDelete')" @confirm="deleteGame()">
      <span class="scenario-icon" aria-hidden="true" :style="{ '--icon': icon }" />
      <span class="confirm-details">
        <strong>{{ game.name }}</strong>
        <span class="confirm-meta">
          {{ game.scenario }} &middot;
          <template v-if="game.waiting">{{ $t('home.waitingForPlayers') }}</template>
          <template v-else>{{ $t('game.round', { round: game.round }) }} &middot; {{ $t(phase) }}</template>
        </span>
        <span v-if="game.seats?.length" class="confirm-meta">{{ game.seats.map((seat) => seat.name).join(', ') }}</span>
      </span>
    </ConfirmDelete>
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

.scenario-icon {
  flex: none;
  width: 36px;
  height: 36px;
  background: var(--accent);
  mask: var(--icon) center / contain no-repeat;
}

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
.badge:is(.easy, .victory) { border: 1px solid var(--box-border); color: var(--text-muted); padding: 1px 7px; }
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
.seat { display: flex; flex-direction: column; gap: 4px; }
/* under the heroes, the player and their threat */
.seat-caption { display: flex; align-items: center; justify-content: space-between; gap: 6px; }
.seat.eliminated { opacity: 0.45; }
/* as a deck tile shows its heroes: their art side by side, over a bar in each one's
   sphere colour */
.heroes {
  display: grid;
  grid-auto-flow: column;
  grid-auto-columns: 40px;
  height: 60px;
  border-radius: 4px;
  overflow: hidden;
  background: var(--surface-2);
  box-shadow: 1px 1px 5px var(--shadow);
}
/* the art sits in the top half of a card, right of the stats */
.hero {
  --sphere-color: var(--neutral);
  background-size: 210% auto;
  background-position: 62% 0;
  border-bottom: 3px solid var(--sphere-color);
}
.hero + .hero { box-shadow: -1px 0 0 rgb(0 0 0 / 0.5); }
.hero.leadership { --sphere-color: var(--leadership); }
.hero.lore { --sphere-color: var(--lore); }
.hero.spirit { --sphere-color: var(--spirit); }
.hero.tactics { --sphere-color: var(--tactics); }
.seat-name { color: var(--text); font-size: 0.85em; font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.seat-threat { display: flex; align-items: center; gap: 2px; color: var(--threat-red); font-size: 0.85em; font-weight: 700; }
.seat-threat .icon {
  display: inline-block;
  width: 1em;
  height: 1em;
  background: currentColor;
  mask: var(--icon) center / contain no-repeat;
}

.confirm-details { display: flex; flex-direction: column; gap: 2px; min-width: 0; }
.confirm-meta { color: var(--text-muted); font-size: 0.85em; }
</style>
