<script setup lang="ts">
import CardBack from './CardBack.vue'
import DiscardPile from './DiscardPile.vue'
import GameCard from './GameCard.vue'
import type { CardView, ChoiceView, ScenarioView } from '../types'
import { useDebug } from '../composables/useDebug'

const { scenario, choices, flipping } = defineProps<{
  scenario: ScenarioView
  choices: ChoiceView[]
  flipping: ReadonlySet<string>
  /** Who searches the encounter deck in debug mode. */
  playerId: string | null
}>()
const emit = defineEmits<{ choose: [index: number] }>()
const { active: debugging, send } = useDebug()

const isFlipping = (card: CardView) => flipping.has(card.id)

// the heading's four settings, on-course first (Dream-chaser rules)
const settings = ['sunny', 'cloudy', 'rainy', 'stormy'].map((key) => ({
  key,
  icon: { '--icon': `url('/images/icons/game/heading-${key}.png')` },
}))
</script>

<template>
  <section class="band scenario">
    <span class="band-label">{{ $t('game.bands.scenario') }}</span>
    <DiscardPile :cards="scenario.discard" back="encounter" :choices="choices" @choose="emit('choose', $event)" data-pile="discard-encounter" />
    <div class="deck">
      <CardBack :count="scenario.encounterDeck" back="encounter" data-pile="deck-encounter" />
      <div v-if="debugging && playerId" class="deck-debug">
        <button type="button" :disabled="scenario.encounterDeck === 0" @click="send({ tag: 'RevealEncounterCard' })">{{ $t('game.debug.reveal') }}</button>
        <button type="button" :disabled="scenario.encounterDeck === 0" @click="send({ tag: 'DebugSearchEncounterDeck', contents: playerId })">{{ $t('game.debug.search') }}</button>
      </div>
    </div>
    <!-- the Corsair deck beside the encounter deck, with its own discard pile -->
    <template v-if="scenario.corsairDeck !== null">
      <DiscardPile :cards="scenario.corsairDiscard" back="encounter" :choices="choices" @choose="emit('choose', $event)" data-pile="discard-corsair" />
      <CardBack :count="scenario.corsairDeck" back="encounter" :title="$t('log.corsairDeck', { count: scenario.corsairDeck })" data-pile="deck-corsair" />
    </template>
    <!-- the Caves deck beside the encounter deck (Ered Mithrin) -->
    <CardBack v-if="scenario.cavesDeck > 0" :count="scenario.cavesDeck" back="encounter" :title="$t('log.cavesDeck', { count: scenario.cavesDeck })" data-pile="deck-caves" />
    <!-- the Power of Mordor deck beside the quest deck (The Temple of Doom, The Fortress of Nurn) -->
    <CardBack v-if="scenario.powerOfMordorDeck > 0" :count="scenario.powerOfMordorDeck" back="encounter" :title="$t('log.powerOfMordorDeck', { count: scenario.powerOfMordorDeck })" data-pile="deck-power-of-mordor" />
    <CardBack v-if="scenario.sackDeck > 0" :count="scenario.sackDeck" back="encounter" :title="$t('log.sackDeck', { count: scenario.sackDeck })" data-pile="deck-sack" />
    <!-- the Burden deck beside the quest deck (Flight to the Ford) -->
    <CardBack v-if="scenario.burdenDeck > 0" :count="scenario.burdenDeck" back="encounter" :title="$t('log.burdenDeck', { count: scenario.burdenDeck })" data-pile="deck-burden" />
    <div v-if="scenario.investigation" class="investigation" data-zone="investigation">
      <span class="heading-label">{{ $t('game.investigationDeck') }}</span>
      <CardBack :count="scenario.investigation.size" back="encounter" :title="$t('log.investigationDeck', { count: scenario.investigation.size })" data-pile="deck-investigation" />
      <details v-if="scenario.investigation.clues.some((entry) => entry.cards.length)">
        <summary>{{ $t('game.investigationClues') }}</summary>
        <div v-for="entry in scenario.investigation.clues" :key="entry.playerId" class="clue-notes">
          <span>{{ entry.player }}</span>
          <div class="clue-cards">
            <GameCard v-for="clue in entry.cards" :key="clue.id" :card="clue" :choices="[]" />
          </div>
        </div>
      </details>
    </div>
    <!-- the side quest deck (Fire in the Night) -->
    <CardBack v-if="scenario.sideQuestDeck > 0" :count="scenario.sideQuestDeck" back="encounter" :title="$t('log.sideQuestDeck', { count: scenario.sideQuestDeck })" data-pile="deck-side-quest" />
    <!-- the Evil Creatures deck, with its own discard pile (Journey Up the Anduin) -->
    <template v-if="scenario.evilCreaturesDeck !== null">
      <DiscardPile :cards="scenario.evilCreaturesDiscard" back="encounter" :choices="choices" @choose="emit('choose', $event)" data-pile="discard-evil-creatures" />
      <CardBack :count="scenario.evilCreaturesDeck" back="encounter" :title="$t('log.evilCreaturesDeck', { count: scenario.evilCreaturesDeck })" data-pile="deck-evil-creatures" />
    </template>
    <div v-if="scenario.heading" class="heading" :title="$t(`log.heading.${scenario.heading}`)">
      <span class="heading-label">{{ $t('game.heading') }}</span>
      <ol class="heading-track">
        <li v-for="s in settings" :key="s.key" :class="{ current: s.key === scenario.heading }"><span class="icon" :style="s.icon" /></li>
      </ol>
    </div>
    <div v-if="scenario.temperature != null" class="heading" data-tracker="temperature">
      <span class="heading-label">{{ $t('game.temperature') }}</span>
      <span class="temperature">{{ scenario.temperature }}</span>
    </div>
    <div v-if="scenario.ringBearersLife != null" class="heading" data-tracker="ring-bearers-life">
      <span class="heading-label">{{ $t('game.ringBearersLife') }}</span>
      <span class="temperature">{{ scenario.ringBearersLife }}</span>
    </div>
    <div v-if="scenario.pursuit != null" class="heading" data-tracker="pursuit">
      <span class="heading-label">{{ $t('game.pursuit') }}</span>
      <span class="temperature">{{ scenario.pursuit }}</span>
    </div>
    <GameCard v-if="scenario.nightmareCard" :card="scenario.nightmareCard" :choices="choices" @choose="emit('choose', $event)" />
    <!-- in campaign mode, the campaign card next to the quest deck -->
    <GameCard v-if="scenario.campaignCard" :card="scenario.campaignCard" :choices="choices" @choose="emit('choose', $event)" />
    <GameCard
      v-if="scenario.quest"
      :card="scenario.quest"
      :attachments="scenario.questAttachments"
      rotated
      :choices="choices"
      :flipping="isFlipping(scenario.quest)"
      @choose="emit('choose', $event)"
    />
    <!-- the Island Map in its 3x5 grid, in place of the active location's slot -->
    <div v-if="scenario.islandMap.length" class="island-map">
      <div
        v-for="cell in scenario.islandMap"
        :key="cell.card.id"
        class="island-cell"
        :class="{ active: cell.active, staged: cell.staged }"
        :style="{ gridColumn: cell.column + 1, gridRow: cell.row + 1 }"
      >
        <GameCard :card="cell.card" :attachments="cell.attachments" :choices="choices" :flipping="isFlipping(cell.card)" @choose="emit('choose', $event)" />
      </div>
    </div>
    <GameCard
      v-else-if="scenario.activeLocation"
      :card="scenario.activeLocation"
      :attachments="scenario.activeLocationAttachments"
      :choices="choices"
      :flipping="isFlipping(scenario.activeLocation)"
      @choose="emit('choose', $event)"
    />
    <!-- where the travelled-to location goes, kept when there is none -->
    <div v-else class="location-placeholder">{{ $t('game.noActiveLocation') }}</div>
    <GameCard
      v-for="other in scenario.otherActiveLocations"
      :key="other.card.id"
      :card="other.card"
      :attachments="other.attachments"
      :choices="choices"
      :flipping="isFlipping(other.card)"
      @choose="emit('choose', $event)"
    />
    <!-- the Stormcaller's area: its own quest stage, and the cards not yet in the staging area -->
    <div v-if="scenario.stormcallerStage" class="stormcaller-area">
      <span class="heading-label">{{ $t('log.stormcallerArea') }}</span>
      <div class="stormcaller-cards">
        <GameCard :card="scenario.stormcallerStage" rotated :choices="choices" @choose="emit('choose', $event)" />
        <GameCard
          v-for="c in scenario.stormcallerArea"
          :key="c.id"
          :card="c"
          :choices="choices"
          :flipping="isFlipping(c)"
          @choose="emit('choose', $event)"
        />
      </div>
    </div>
    <!-- the circuit: stages 1 to 3 left to right, 4 to 6 above them right to left, so 4 is
         above 3, with The Challengers and The Wainriders at their stages (Challenge of the Wainriders) -->
    <div v-if="scenario.circuit" class="stormcaller-area circuit" data-zone="circuit">
      <span class="heading-label">{{ $t('log.circuit') }}</span>
      <div class="circuit-grid">
        <div
          v-for="s in scenario.circuit"
          :key="s.card.id"
          class="circuit-stage"
          :class="{ current: s.challengersLap !== null }"
          :style="{ gridColumn: s.stage <= 3 ? s.stage : 7 - s.stage, gridRow: s.stage <= 3 ? 2 : 1 }"
          :data-stage="s.stage"
        >
          <GameCard :card="s.card" rotated :choices="choices" @choose="emit('choose', $event)" />
          <div class="circuit-markers">
            <span v-if="s.challengersLap !== null" class="good">{{ $t('log.theChallengers') }} · {{ s.challengersLap }}/2</span>
            <span v-if="s.wainridersLap !== null" class="bad">{{ $t('log.theWainriders') }} · {{ s.wainridersLap }}/2</span>
          </div>
          <div v-if="s.objectives.length" class="circuit-objectives">
            <GameCard v-for="o in s.objectives" :key="o.id" :card="o" :choices="choices" @choose="emit('choose', $event)" />
          </div>
        </div>
      </div>
    </div>
    <!-- the riddle area: Riddles in the Dark and Gollum, apart from the quest and staging area -->
    <div v-if="scenario.riddleStage" class="stormcaller-area">
      <span class="heading-label">{{ $t('log.riddleArea') }}</span>
      <div class="stormcaller-cards">
        <GameCard :card="scenario.riddleStage" rotated :choices="choices" @choose="emit('choose', $event)" />
        <GameCard
          v-for="c in scenario.riddleArea"
          :key="c.id"
          :card="c"
          :choices="choices"
          :flipping="isFlipping(c)"
          @choose="emit('choose', $event)"
        />
      </div>
    </div>
    <!-- the Orcs' area: their own quest stage, and the cards not in the staging area -->
    <div v-if="scenario.orcsStage" class="stormcaller-area">
      <span class="heading-label">{{ $t('log.orcsArea') }}</span>
      <div class="stormcaller-cards">
        <GameCard :card="scenario.orcsStage" rotated :choices="choices" @choose="emit('choose', $event)" />
        <GameCard
          v-for="c in scenario.orcsArea"
          :key="c.id"
          :card="c"
          :choices="choices"
          :flipping="isFlipping(c)"
          @choose="emit('choose', $event)"
        />
      </div>
    </div>
  </section>
</template>

<style scoped>
.scenario { flex: 0 0 auto; }
.deck { display: flex; flex-direction: column; gap: 4px; }
.investigation { display: flex; flex-direction: column; gap: 6px; }
.investigation summary { cursor: pointer; font-size: 0.65rem; }
.clue-notes { display: flex; flex-direction: column; gap: 4px; font-size: 0.65rem; padding: 6px; }
.clue-cards { display: flex; flex-wrap: wrap; gap: 4px; max-width: calc(var(--card-width) * 3 + 8px); }
.heading { display: flex; flex-direction: column; align-items: center; gap: 4px; font-size: 0.6rem; letter-spacing: 0.08em; text-transform: uppercase; color: var(--text-muted); }
.temperature { font-size: 1.2rem; color: var(--text); }
.heading-track { display: flex; flex-direction: column; gap: 2px; margin: 0; padding: 0; list-style: none; }
.heading-track li { width: 1.6rem; height: 1.6rem; display: grid; place-items: center; border: 1px solid var(--line-strong); border-radius: 4px; font-size: 0.9rem; opacity: 0.4; }
.heading-track li.current { opacity: 1; border-color: currentColor; color: var(--text); }
.stormcaller-area { display: flex; flex-direction: column; gap: 4px; padding-left: 8px; border-left: 1px dashed var(--line-strong); font-size: 0.6rem; letter-spacing: 0.08em; text-transform: uppercase; color: var(--text-muted); }
.stormcaller-cards { display: flex; gap: 6px; align-items: flex-start; }
.circuit-grid { display: grid; grid-template-columns: repeat(3, auto); grid-template-rows: repeat(2, auto); gap: 6px; --card-width: 4.5rem; }
.circuit-stage { display: flex; flex-direction: column; align-items: center; gap: 2px; padding: 2px; border: 1px solid transparent; border-radius: 6px; }
.circuit-stage.current { border-color: var(--text); }
.circuit-markers { display: flex; flex-direction: column; align-items: center; gap: 1px; text-transform: none; letter-spacing: 0; }
.circuit-markers .good { color: var(--success); }
.circuit-markers .bad { color: var(--danger); }
.circuit-objectives { display: flex; gap: 2px; }
.island-map { display: grid; grid-template-columns: repeat(5, auto); grid-template-rows: repeat(3, auto); gap: 6px; }
.island-cell { border: 2px solid transparent; border-radius: 8px; padding: 2px; opacity: 0.75; }
.island-cell.staged { opacity: 1; border-color: var(--line-strong); }
.island-cell.active { opacity: 1; border-color: var(--text); }
.location-placeholder {
  display: flex;
  align-items: center;
  justify-content: center;
  width: var(--card-width);
  aspect-ratio: var(--card-aspect);
  padding: 6px;
  border: 1px dashed var(--line-strong);
  border-radius: 6px;
  color: var(--text-muted);
  font-size: 0.6rem;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  text-align: center;
}
</style>
