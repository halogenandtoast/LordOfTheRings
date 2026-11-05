<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { useCardOverlay } from '../composables/useCardOverlay'
import { useServerText } from '../i18n'
import type { ChoiceView, ScoreView, QuestionView } from '../types'

const tr = useServerText()
const overlay = useCardOverlay()

const { question, outcome = null, victoryPoints = 0, score } = defineProps<{
  question: QuestionView | null
  outcome?: 'WON' | 'LOST' | null
  score?: ScoreView
  victoryPoints?: number
}>()
const emit = defineEmits<{ choose: [index: number] }>()

const labelChoices = computed<ChoiceView[]>(() =>
  // an amount is picked in its own panel
  question?.amount ? [] : (question?.choices ?? []).filter((choice) => choice.targetId === null),
)
const targeted = computed(() => (question?.choices ?? []).length - labelChoices.value.length)
const choiceSearch = ref('')
watch(() => question, () => { choiceSearch.value = '' })
const visibleLabelChoices = computed(() => {
  const query = choiceSearch.value.trim().toLocaleLowerCase()
  return query ? labelChoices.value.filter((choice) => tr(choice.label).toLocaleLowerCase().includes(query)) : labelChoices.value
})
</script>

<template>
  <div class="question" :class="{ idle: !question }">
    <template v-if="question">
      <p class="prompt">{{ tr(question.prompt) }}</p>
      <input
        v-if="labelChoices.length > 20"
        v-model="choiceSearch"
        class="choice-search"
        type="search"
        :aria-label="$t('game.prompt.searchChoices')"
        :placeholder="$t('game.prompt.searchChoices')"
      />
      <div class="options">
        <button
          v-for="choice in visibleLabelChoices"
          :key="choice.index"
          type="button"
          @click="choice.card && overlay.hide(); emit('choose', choice.index)"
          @pointerenter="(e: PointerEvent) => choice.card && e.pointerType === 'mouse' && overlay.show(choice.card, e.currentTarget as HTMLElement)"
          @pointerleave="(e: PointerEvent) => choice.card && e.pointerType === 'mouse' && overlay.hide()"
        >
          {{ tr(choice.label) }}
        </button>
      </div>
      <p v-if="targeted" class="hint"><span class="swatch interactive"></span>{{ $t('game.prompt.orClickHighlighted') }}</p>
      <p v-if="question.sourceId || question.targetIds.length" class="legend">
        <span v-if="question.sourceId"><span class="swatch source"></span>{{ $t('game.prompt.source') }}</span>
        <span v-if="question.targetIds.length"><span class="swatch target"></span>{{ $t('game.prompt.target') }}</span>
      </p>
    </template>
    <template v-else-if="outcome">
      <p class="prompt outcome" :class="outcome.toLowerCase()">
        {{ $t(outcome === 'WON' ? 'game.prompt.playersWin' : 'game.prompt.playersLose') }}
        &middot; {{ $t('game.prompt.victoryPoints', { value: victoryPoints }) }}
      </p>
      <dl v-if="outcome === 'WON' && score" class="score">
        <template v-for="key in (['threat', 'deadHeroes', 'damage', 'rounds', 'victoryPoints', 'total'] as const)" :key="key">
          <dt>{{ $t(`game.score.${key}`) }}</dt><dd>{{ key === 'victoryPoints' ? '−' : '' }}{{ score[key] }}</dd>
        </template>
      </dl>
      <p v-if="outcome === 'WON' && score" class="hint">{{ $t('game.score.lowerIsBetter') }}</p>
    </template>
    <p v-else class="prompt idle-text">{{ $t('game.prompt.waiting') }}</p>
  </div>
</template>

<style scoped>
.choice-search {
  width: 100%;
  box-sizing: border-box;
  padding: 0.4rem 0.5rem;
  color: var(--text);
  background: var(--panel);
  border: 1px solid var(--line-strong);
  border-radius: 4px;
  font: inherit;
  font-size: 0.8rem;
}
.choice-search:focus-visible { outline: 2px solid var(--accent); outline-offset: 1px; }
.score { display: grid; grid-template-columns: 1fr auto; gap: 0.25rem 1rem; margin: 0; font-size: 0.8rem; }
.score dd { margin: 0; text-align: right; font-variant-numeric: tabular-nums; }
.score dt:nth-last-of-type(1), .score dd:last-child { border-top: 1px solid var(--line); padding-top: 0.3rem; font-weight: 700; }
.swatch {
  display: inline-block;
  width: 0.6rem;
  height: 0.6rem;
  border-radius: 50%;
  margin-right: 0.35rem;
  vertical-align: middle;
}
.swatch.interactive { background: var(--interactive); }
.swatch.source { background: var(--source); }
.swatch.target { background: var(--target); }
.legend {
  display: flex;
  gap: 0.8rem;
  margin: 0.25rem 0 0;
  font-size: 0.7rem;
  color: var(--text-muted);
  text-transform: uppercase;
  letter-spacing: 0.06em;
}
.question {
  flex: 0 1 20rem;
  min-width: 12rem;
  align-self: stretch;
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
  padding: 0.5rem 0.6rem;
  background: var(--panel);
  border: 1px solid var(--line);
  border-radius: 6px;
  overflow-y: auto;
}
.question.idle:not(:has(.outcome)) { opacity: 0.4; }
.outcome { font-weight: 700; }
.outcome.won { color: var(--accent); }
.outcome.lost { color: var(--danger); }
.prompt { margin: 0; font-size: 0.8rem; line-height: 1.35; }
.idle-text { opacity: 0.6; font-style: italic; }
.options { display: flex; flex-direction: column; gap: 4px; }
.options button {
  font: inherit;
  font-size: 0.75rem;
  text-align: left;
  padding: 0.3rem 0.5rem;
  border: 1px solid var(--line);
  border-radius: 4px;
  background: var(--subtle-tint);
  color: inherit;
  cursor: pointer;
}
.options button:hover { border-color: var(--accent); background: var(--highlight-tint); }
/* finger-sized answers */
@media (pointer: coarse) {
  .options button { min-height: 44px; font-size: 0.9rem; }
}
.hint { margin: 0; font-size: 0.65rem; opacity: 0.5; font-style: italic; }
</style>
