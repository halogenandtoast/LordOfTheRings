<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref } from 'vue'
import { useRoute } from 'vue-router'
import type { CardView } from '../types'
import CardBand from '../components/CardBand.vue'
import CardOverlay from '../components/CardOverlay.vue'
import Log from '../components/Log.vue'
import GameBar from '../components/GameBar.vue'
import PlayerArea from '../components/PlayerArea.vue'
import Prompt from '../components/Prompt.vue'
import Scenario from '../components/Scenario.vue'
import ThreatDial from '../components/ThreatDial.vue'
import { exportGame } from '../api'
import { useGame } from '../composables/useGame'
import { provideHighlights } from '../composables/useHighlights'

const route = useRoute()
const {
  error, game, log, announcement, flipping,
  started, question, scenario, players, outcome, answer, reset, undo, canUndo,
} = useGame(route.params.id as string)
const showSidebar = ref(true)

const choices = computed(() => question.value?.choices ?? [])
provideHighlights(() => question.value)
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

// Cmd/Ctrl+Z steps back one answer, unless you are typing somewhere.
const onKeydown = (event: KeyboardEvent) => {
  const typing = event.target instanceof HTMLInputElement || event.target instanceof HTMLTextAreaElement
  if (!typing && (event.metaKey || event.ctrlKey) && !event.shiftKey && event.key.toLowerCase() === 'z' && canUndo.value) {
    event.preventDefault()
    undo()
  }
}
onMounted(() => document.addEventListener('keydown', onKeydown))
onBeforeUnmount(() => document.removeEventListener('keydown', onKeydown))

const choose = (index: number) => {
  const asked = question.value
  if (asked) {
    answer(asked.askId, index)
  }
}
</script>

<template>
  <div class="game-view">
  <GameBar
    :can-undo="canUndo"
    :show-sidebar="showSidebar"
    @reset="reset"
    @undo="undo($event)"
    @export="exportGame(route.params.id as string)"
    @toggle-sidebar="showSidebar = !showSidebar"
  />
  <div class="layout" :class="{ 'no-sidebar': !showSidebar }">
    <CardOverlay />
    <div class="game">


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

    <aside v-if="showSidebar" class="sidebar">
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
  </div>
</template>

<style scoped>
.game-view { display: flex; flex-direction: column; flex: 1; min-height: 0; }
.layout {
  display: grid;
  grid-template-columns: 1fr 280px;
  gap: var(--gap);
  flex: 1;
  min-height: 0;
  padding: var(--gap);
}
.layout.no-sidebar { grid-template-columns: 1fr; }

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


</style>
