<script setup lang="ts">
import { computed, ref } from 'vue'
import type { CardView } from './types'
import CardBand from './components/CardBand.vue'
import CardOverlay from './components/CardOverlay.vue'
import Log from './components/Log.vue'
import MenuBar from './components/MenuBar.vue'
import PlayerArea from './components/PlayerArea.vue'
import Prompt from './components/Prompt.vue'
import Scenario from './components/Scenario.vue'
import ThreatDial from './components/ThreatDial.vue'
import { useGame } from './composables/useGame'

const {
  status, error, game, log, announcement, flipping,
  started, question, scenario, players, outcome, startGame, answer, reset, undo, canUndo,
} = useGame()
const name = ref('Player')

const choices = computed(() => question.value?.choices ?? [])
const you = computed(() => players.value[0] ?? null)
const engaged = computed(() => players.value.flatMap((player) => player.engaged))
const cardsInPlay = computed(() => players.value.flatMap((player) => player.inPlay))
const attachments = computed(() => cardsInPlay.value.filter((card) => card.attachedTo !== null))

const withAttachments = (hosts: CardView[]) => [
  ...hosts,
  ...attachments.value.filter((attached) => hosts.some((host) => host.id === attached.attachedTo)),
]

const heroes = computed(() =>
  withAttachments(cardsInPlay.value.filter((card) => card.type === 'Hero')),
)
const inPlay = computed(() =>
  withAttachments(
    cardsInPlay.value.filter((card) => card.type !== 'Hero' && card.attachedTo === null),
  ),
)
const engagedWithAttachments = computed(() => withAttachments(engaged.value))

const choose = (index: number) => {
  const asked = question.value
  if (asked) {
    answer(asked.askId, index)
  }
}
</script>

<template>
  <div id="app">
    <CardOverlay />
    <div class="game">
      <MenuBar :disabled="!started" :can-undo="canUndo" @reset="reset" @undo="undo" />

      <section v-if="!started" class="start">
        <label>
          Your name
          <input v-model="name" type="text" @keyup.enter="startGame(name)" />
        </label>
        <button type="button" :disabled="status !== 'open'" @click="startGame(name)">Start a game</button>
      </section>

      <template v-if="started">
        <Scenario v-if="scenario" :scenario="scenario" :choices="choices" :flipping="flipping" @choose="choose" />

        <CardBand
          v-if="scenario"
          label="staging"
          :cards="scenario.staging"
          :choices="choices"
          :flipping="flipping"
          @choose="choose"
        />

        <CardBand
          label="threat area"
          align="bottom"
          :cards="engagedWithAttachments"
          :choices="choices"
          :flipping="flipping"
          @choose="choose"
        />

        <section class="band play">
          <div class="hero-column">
            <ThreatDial v-if="you" :threat="you.threat" />
            <CardBand
              class="heroes"
              :cards="heroes"
              :choices="choices"
              :flipping="flipping"
              align="bottom"
              @choose="choose"
            />
          </div>
          <CardBand
            label="play area"
            :cards="inPlay"
            :choices="choices"
            :flipping="flipping"
            align="bottom"
            @choose="choose"
          />
        </section>

        <PlayerArea
          v-if="you"
          :player="you"
          :choices="choices"
          :flipping="flipping"
          :question="question"
          @choose="choose"
        />
      </template>
    </div>

    <aside class="sidebar">
      <h1>The Lord of the Rings</h1>
      <p class="status" :data-status="status">{{ status }}</p>
      <p v-if="error" class="error">{{ error }}</p>
      <Prompt
        v-if="started"
        :question="question"
        :outcome="outcome"
        :round="game?.round ?? 0"
        :phase="game?.phase ?? null"
      />
      <p v-if="announcement" class="announcement">{{ announcement }}</p>
      <Log :log="log" />
    </aside>
  </div>
</template>

<style scoped>
#app {
  display: grid;
  grid-template-columns: 1fr 280px;
  gap: var(--gap);
  height: 100%;
  padding: var(--gap);
}

.band.play { flex: 1 1 auto; gap: calc(var(--gap) * 2); align-items: stretch; padding: 0; }
.hero-column {
  display: flex;
  flex-direction: column;
  gap: var(--gap);
  align-items: center;
  justify-content: flex-end;
  flex: 0 0 auto;
  border-right: 1px solid var(--line);
  padding-right: calc(var(--gap) * 1.5);
}
.band.play .heroes { flex: 0 0 auto; padding: 0; }

.game {
  display: flex;
  flex-direction: column;
  gap: var(--gap);
  min-width: 0;
  min-height: 0;
}

.sidebar {
  display: flex;
  flex-direction: column;
  gap: 0.6rem;
  min-height: 0;
}
h1 { margin: 0; font-size: 1rem; letter-spacing: 0.02em; }
.status { margin: 0; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.08em; opacity: 0.5; }
.status[data-status='closed'] { color: var(--danger); }
.error { margin: 0; color: var(--danger); font-size: 0.8rem; }

.announcement {
  margin: 0;
  font-size: 0.75rem;
  font-style: italic;
  opacity: 0.8;
  border-left: 2px solid var(--accent);
  padding-left: 0.5rem;
  white-space: pre-line;
}


.start { display: flex; gap: var(--gap); align-items: flex-end; flex: 0 0 auto; }
label { display: flex; flex-direction: column; gap: 0.25rem; font-size: 0.75rem; opacity: 0.7; }
input, .start button {
  font: inherit;
  padding: 0.4rem 0.6rem;
  border: 1px solid var(--line);
  border-radius: 5px;
  background: var(--panel);
  color: inherit;
}
.start button { cursor: pointer; }
.start button:disabled { opacity: 0.5; cursor: not-allowed; }
</style>
