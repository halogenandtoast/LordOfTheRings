<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, ref, watch } from 'vue'
import { useRoute } from 'vue-router'
import { cardDrag, type CardView } from '../types'
import { isKey, useServerText } from '../i18n'
import CardBand from '../components/CardBand.vue'
import CardOverlay from '../components/CardOverlay.vue'
import Log from '../components/Log.vue'
import GameBar from '../components/GameBar.vue'
import PlayerArea from '../components/PlayerArea.vue'
import PhaseTracker from '../components/PhaseTracker.vue'
import Prompt from '../components/Prompt.vue'
import QuestTally from '../components/QuestTally.vue'
import QuestionZone from '../components/QuestionZone.vue'
import SearchPanel from '../components/SearchPanel.vue'
import Scenario from '../components/Scenario.vue'
import SeatTabs from '../components/SeatTabs.vue'
import Lobby from '../components/Lobby.vue'
import { currentUser } from '../auth'
import ThreatDial from '../components/ThreatDial.vue'
import FileBug from '../components/FileBug.vue'
import { exportGame } from '../api'
import { useGame } from '../composables/useGame'
import { provideDebug } from '../composables/useDebug'
import { provideHighlights } from '../composables/useHighlights'

const route = useRoute()
const tr = useServerText()
const spectate = route.meta.spectate === true
const {
  error, game, log, announcement, flipping,
  started, question: asked, scenario, players, table, outcome, answer, reset, undo, canUndo, debug, join, leave, start,
} = useGame(route.params.id as string, spectate)
// With friends you play your own seat; otherwise every seat is yours.
const withFriends = computed(() => table.value?.withFriends ?? false)
const guest = computed(() => withFriends.value && table.value?.owner !== currentUser.value?.id)
const mySeat = computed(() => {
  const position = table.value?.seated.find((seat) => seat.userId === currentUser.value?.id)?.position
  return withFriends.value && position !== undefined ? players.value[position]?.playerId ?? null : null
})
// someone else's turn to answer, as a spectator always sees it: what is asked, with nothing to choose
const waitingOn = computed(() =>
  asked.value && (spectate || (withFriends.value && asked.value.playerId !== mySeat.value))
    ? players.value.find((player) => player.playerId === asked.value?.playerId) ?? null
    : null,
)
const question = computed(() => (waitingOn.value && asked.value ? { ...asked.value, choices: [] } : asked.value))
const { active: debugging, send: sendDebug } = provideDebug(debug)
const filingBug = ref(false)
const bugDescription = ref('')
const fileBugAbout = (description = '') => {
  bugDescription.value = description
  filingBug.value = true
}
// on a narrower screen the sidebar is a drawer over the board, so it starts shut
const showSidebar = ref(!matchMedia('(width < 1100px), (height < 500px)').matches)

const choices = computed(() => question.value?.choices ?? [])
provideHighlights(() => question.value, () => dragging.value?.hosts ?? [])
// The seat whose board is shown, picked from the tabs along the foot. It starts as
// yours, and follows whoever is asked when every seat is yours.
const seatId = ref<string | null>(null)
const you = computed(
  () => players.value.find((player) => player.playerId === (seatId.value ?? mySeat.value)) ?? players.value[0] ?? null,
)
watch(() => question.value?.playerId, (asked) => {
  if (asked && !withFriends.value) seatId.value = asked
}, { immediate: true })

const cardsInPlay = computed(() => you.value?.inPlay ?? [])
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
const engaged = computed(() => you.value?.engaged ?? [])

// Cmd/Ctrl+Z steps back one answer and D toggles debug mode, unless you are typing somewhere.
const onKeydown = (event: KeyboardEvent) => {
  if (spectate) return
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
    answer(asked.askId, index, asked.playerId)
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
  if (!isKey(asked.prompt, 'label.attachTo') || asked.sourceId !== preset.cardId) {
    // still paying for it; anything else means the drop has been used up
    if (!isKey(asked.prompt, 'label.payMore') && !isKey(asked.prompt, 'label.payFromDifferentHero')) presetHost.value = null
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
    :spectate="spectate"
    :guest="guest"
    @reset="reset"
    @undo="undo($event)"
    @export="exportGame(route.params.id as string)"
    @file-bug="fileBugAbout()"
    @toggle-sidebar="showSidebar = !showSidebar"
    @toggle-debug="debugging = !debugging"
  />
  <FileBug :open="filingBug" :game-id="route.params.id as string" :description="bugDescription" @close="filingBug = false" />
  <div class="layout" :class="{ 'no-sidebar': !showSidebar }">
    <CardOverlay />
    <SearchPanel v-if="question?.searched" :question="{ ...question, searched: question.searched }" @choose="choose" />
    <div
      class="game"
      @dragstart="onDragStart"
      @dragover="onDragOver"
      @drop="onDrop"
      @dragend="dragging = null"
    >


      <Lobby v-if="!started && table?.withFriends" :table="table" @join="join" @leave="leave" @start="start" />
      <template v-if="started">
        <QuestTally v-if="game?.tally" :tally="game.tally" />
        <Scenario v-if="scenario" :scenario="scenario" :player-id="you?.playerId ?? null" :choices="choices" :flipping="flipping" @choose="choose" />

        <CardBand
          v-if="scenario"
          class="staging"
          :label="$t('game.bands.staging')"
          :cards="scenario.staging"
          :choices="choices"
          :flipping="flipping"
          @choose="choose"
        />

        <!-- in front of the player, as engaged enemies sit on the table -->
        <CardBand
          class="threat-area"
          :label="$t('game.bands.threatArea')"
          :cards="engaged"
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
              <button type="button" :aria-label="$t('game.debug.lowerThreat')" :disabled="you.threat === 0" @click="sendDebug({ tag: 'LowerThreat', contents: [you.playerId, 1] })">−</button>
              <button type="button" :aria-label="$t('game.debug.raiseThreat')" @click="sendDebug({ tag: 'RaiseThreat', contents: [you.playerId, 1] })">+</button>
            </div>
          </div>
          <div class="player-column">
            <CardBand
              class="play-area"
              :class="{ 'drop-zone': dragging && dragging.hosts === null }"
              :label="$t('game.bands.playArea')"
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
        <SeatTabs
          v-if="players.length > 1"
          :players="players"
          :viewing="you?.playerId ?? null"
          :asked="asked?.playerId ?? null"
          :mine="mySeat"
          @view="seatId = $event"
        />
      </template>
    </div>

    <PhaseTracker :phase="game?.phase ?? null" />
    <!-- on a small screen the question sits under the board, always in view -->
    <QuestionZone v-if="started" class="docked-question" :question="question" @choose="choose" />

    <aside v-if="showSidebar" class="sidebar">
      <p v-if="error" class="error">
        {{ tr(error) }}
        <button v-if="!spectate" type="button" class="file-bug-link" @click="fileBugAbout(tr(error))">{{ $t('game.bar.fileBug') }}</button>
      </p>
      <Prompt
        v-if="started"
        :question="question"
        :outcome="outcome"
        :round="game?.round ?? 0"
        :phase="game?.phase ?? null"
      />
      <p v-if="waitingOn && !spectate" class="announcement">{{ $t('game.waitingOn', { name: waitingOn.name }) }}</p>
      <p v-if="announcement" class="announcement">{{ tr(announcement) }}</p>
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
.docked-question { display: none; }

@media (width < 1100px), (height < 500px) {
  .layout, .layout.no-sidebar {
    position: relative;
    grid-template-columns: minmax(0, 1fr);
    grid-template-rows: auto minmax(0, 1fr) auto;
  }
  .layout > .phases { order: -1; }
  .game { margin: 0 0 0 calc(-1 * var(--gap)); padding-top: 0; padding-bottom: 0; }
  .game > .seat-tabs { margin-bottom: 0; bottom: 0; }
  .game :deep(.tally) { margin-top: 0; border-top: 1px solid var(--line); border-radius: 6px; }
  .sidebar {
    position: absolute;
    inset: 0 0 0 auto;
    z-index: 60;
    width: min(320px, 85vw);
    padding: var(--gap);
    background: var(--surface-1);
    border-left: 1px solid var(--line);
    box-shadow: -8px 0 24px var(--shadow);
  }
  .docked-question {
    display: flex;
    order: 1;
    min-width: 0;
    max-height: 40dvh;
  }
}

/* a phone on its side: the question beside the board, which needs all the height there is */
@media (height < 500px) and (width >= 700px) {
  .layout, .layout.no-sidebar {
    grid-template-columns: minmax(0, 1fr) minmax(200px, 30%);
    grid-template-rows: auto minmax(0, 1fr);
  }
  .layout > .phases, .game { grid-column: 1; }
  .game { grid-row: 2; }
  .docked-question { grid-column: 2; grid-row: 1 / -1; max-height: none; }
}

@media (width < 700px) {
  .band.play { flex-direction: column; gap: var(--gap); }
  .hero-column {
    flex-direction: row;
    flex-wrap: wrap;
    align-items: flex-end;
    justify-content: flex-start;
  }
}

/* each zone of the table has its own stripe, kept when empty so it stays a place */
.game > .scenario { background: var(--zone-scenario); }
.game > .staging { background: var(--zone-staging); }
.game > .threat-area { background: var(--zone-threat); }
/* room for a card even when empty; border-box, so the padding and border count toward it */
.game > .staging, .game > .threat-area {
  flex: 0 0 auto;
  padding: var(--gap) 4px;
  min-height: calc(var(--card-width) / var(--card-aspect) + var(--gap) * 2 + 2px);
}
.game > .staging :deep(.band-label), .game > .threat-area :deep(.band-label) { align-self: flex-start; }
.band.play { flex: 1 1 auto; gap: calc(var(--gap) * 2); align-items: stretch; background: var(--zone-play); }
.hero-column {
  display: flex;
  flex-direction: column;
  gap: var(--gap);
  align-items: center;
  justify-content: flex-end;
  flex: 0 0 auto;
}
.band.play .play-area.drop-zone {
  flex: 1 1 auto;
  min-height: calc(var(--card-width) / var(--card-aspect));
  outline: 2px dashed var(--target);
  outline-offset: -2px;
}
.band.play .heroes { flex: 0 0 auto; padding: 0; gap: calc(var(--gap) * 2); }
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
  /* scrolls when the board is taller than the screen; reaches up to the bar so the tally
     can still tuck under it */
  overflow: hidden auto;
  margin-top: calc(-1 * var(--gap));
  padding-top: var(--gap);
  /* out to the layout's left and bottom edges, so the seat tabs can sit in the corner */
  margin-left: calc(-1 * var(--gap));
  padding-left: var(--gap);
  margin-bottom: calc(-1 * var(--gap));
  padding-bottom: var(--gap);
}
/* sticky keeps out of .game's padding unless told to reach into it */
.game > .seat-tabs { margin: var(--gap) 0 calc(-1 * var(--gap)) calc(-1 * var(--gap)); bottom: calc(-1 * var(--gap)); }

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
.file-bug-link { background: none; border: 0; padding: 0; color: inherit; font: inherit; text-decoration: underline; cursor: pointer; }

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
