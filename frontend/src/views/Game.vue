<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useRoute } from 'vue-router'
import { cardDrag, type CardView } from '../types'
import CardBand from '../components/CardBand.vue'
import CardOverlay from '../components/CardOverlay.vue'
import Log from '../components/Log.vue'
import GameBar from '../components/GameBar.vue'
import PlayerArea from '../components/PlayerArea.vue'
import PhaseTracker from '../components/PhaseTracker.vue'
import Prompt from '../components/Prompt.vue'
import QuestTally from '../components/QuestTally.vue'
import Scenario from '../components/Scenario.vue'
import ThreatDial from '../components/ThreatDial.vue'
import { exportGame } from '../api'
import { useGame } from '../composables/useGame'
import { provideDebug } from '../composables/useDebug'
import { provideHighlights } from '../composables/useHighlights'

const route = useRoute()
const {
  error, game, log, announcement, flipping,
  started, question, scenario, players, outcome, answer, reset, undo, canUndo, debug,
} = useGame(route.params.id as string)
const { active: debugging, send: sendDebug } = provideDebug(debug)
const showSidebar = ref(true)

const choices = computed(() => question.value?.choices ?? [])
provideHighlights(() => question.value, () => dragging.value?.hosts ?? [])
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

// Cmd/Ctrl+Z steps back one answer and D toggles debug mode, unless you are typing somewhere.
const onKeydown = (event: KeyboardEvent) => {
  const typing = event.target instanceof HTMLInputElement || event.target instanceof HTMLTextAreaElement
  if (!typing && !event.metaKey && !event.ctrlKey && !event.altKey && event.key.toLowerCase() === 'd') {
    debugging.value = !debugging.value
  }
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

// Dragging a playable hand card plays it: an attachment onto one of the cards it can go
// on, which is then picked as its host once the engine asks (after any payment), anything
// else onto the play area.
const presetHost = ref<{ cardId: string; hostId: string } | null>(null)
const dragging = ref<{ cardId: string; hosts: readonly string[] | null } | null>(null)

const cardIdAt = (event: Event) =>
  (event.target as Element).closest<HTMLElement>('[data-card-id]')?.dataset.cardId

const onDragStart = (event: DragEvent) => {
  const cardId = cardIdAt(event)
  const card = you.value?.hand.find((c) => c.id === cardId)
  if (!card || !event.dataTransfer?.types.includes(cardDrag)) return
  dragging.value = {
    cardId: card.id,
    hosts: card.type === 'Attachment' ? game.value?.attachmentHosts[card.id] ?? [] : null,
  }
}

// The host it would attach to, or the play area, when the drag is over a valid spot.
const dropSpot = (event: DragEvent) => {
  const drag = dragging.value
  if (!drag) return null
  if (drag.hosts === null) return (event.target as Element).closest('.play-area') ? { hostId: null } : null
  const hostId = cardIdAt(event)
  return hostId && drag.hosts.includes(hostId) ? { hostId } : null
}

const onDragOver = (event: DragEvent) => {
  if (dropSpot(event)) event.preventDefault()
}

const onDrop = (event: DragEvent) => {
  const drag = dragging.value
  const spot = dropSpot(event)
  dragging.value = null
  const play = choices.value.find((option) => option.targetId === drag?.cardId)
  if (!drag || !spot || !play) return
  event.preventDefault()
  presetHost.value = spot.hostId ? { cardId: drag.cardId, hostId: spot.hostId } : null
  choose(play.index)
}

watch(question, (asked) => {
  const preset = presetHost.value
  if (!preset || !asked) return
  if (asked.prompt !== 'Attach to' || asked.sourceId !== preset.cardId) {
    // still paying for it; anything else means the drop has been used up
    if (!asked.prompt.startsWith('Pay ')) presetHost.value = null
    return
  }
  presetHost.value = null
  const host = asked.choices.find((option) => option.targetId === preset.hostId)
  if (host) choose(host.index)
})
</script>

<template>
  <div class="game-view">
  <GameBar
    :can-undo="canUndo"
    :show-sidebar="showSidebar"
    :debugging="debugging"
    @reset="reset"
    @undo="undo($event)"
    @export="exportGame(route.params.id as string)"
    @toggle-sidebar="showSidebar = !showSidebar"
    @toggle-debug="debugging = !debugging"
  />
  <div class="layout" :class="{ 'no-sidebar': !showSidebar }">
    <CardOverlay />
    <div
      class="game"
      @dragstart="onDragStart"
      @dragover="onDragOver"
      @drop="onDrop"
      @dragend="dragging = null"
    >


      <template v-if="started">
        <QuestTally v-if="game?.tally" :tally="game.tally" />
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
            <CardBand
              class="heroes"
              :cards="heroes"
              :choices="choices"
              :flipping="flipping"
              align="bottom"
              @choose="choose"
            />
            <ThreatDial v-if="you" :threat="you.threat" />
            <div v-if="you && debugging" class="debug-threat">
              <button type="button" aria-label="Lower threat" :disabled="you.threat === 0" @click="sendDebug({ tag: 'LowerThreat', contents: [you.playerId, 1] })">−</button>
              <button type="button" aria-label="Raise threat" @click="sendDebug({ tag: 'RaiseThreat', contents: [you.playerId, 1] })">+</button>
            </div>
          </div>
          <div class="player-column">
            <CardBand
              class="play-area"
              :class="{ 'drop-zone': dragging && dragging.hosts === null }"
              label="play area"
              :cards="inPlay"
              :choices="choices"
              :flipping="flipping"
              align="bottom"
              @choose="choose"
            />
            <PlayerArea
              v-if="you"
              :player="you"
              :choices="choices"
              :flipping="flipping"
              :question="question"
              @choose="choose"
            />
          </div>
        </section>
      </template>
    </div>

    <PhaseTracker :phase="game?.phase ?? null" />

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
  grid-template-columns: 1fr auto 280px;
  gap: var(--gap);
  flex: 1;
  min-height: 0;
  padding: var(--gap);
}
.layout.no-sidebar { grid-template-columns: 1fr auto; }

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
.band.play .play-area.drop-zone {
  flex: 1 1 auto;
  min-height: calc(var(--card-width) / var(--card-aspect));
  outline: 2px dashed var(--target);
  outline-offset: -2px;
}
.band.play .heroes { flex: 0 0 auto; padding: 0; }
/* the play area over the hand, both beside the heroes */
.player-column {
  display: flex;
  flex-direction: column;
  gap: var(--gap);
  flex: 1 1 0;
  min-width: 0;
}
.player-column .play-area { flex: 1 1 auto; }

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
.debug-threat { display: flex; gap: var(--gap); }
.debug-threat button { width: 28px; height: 22px; padding: 0; border: 1px solid var(--line); border-radius: 4px; background: none; color: inherit; cursor: pointer; }
.debug-threat button:disabled { opacity: 0.35; cursor: default; }
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
