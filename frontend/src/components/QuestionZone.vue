<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { useCardOverlay } from '../composables/useCardOverlay'
import { useServerText } from '../i18n'
import type { ChoiceView, ScoreView, QuestionView } from '../types'

const tr = useServerText()
const overlay = useCardOverlay()

const { question, outcome = null, victoryPoints = 0, score, campaignId = null } = defineProps<{
  question: QuestionView | null
  outcome?: 'WON' | 'LOST' | null
  /** in campaign mode, the way on to the campaign page once the scenario is over */
  campaignId?: string | null
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

// Use the server's stable translation key, so sphere styling works in every language.
const sphereOf = (label: string) =>
  /^\$label\.[\w.]+\.(leadership|lore|spirit|tactics|baggins|fellowship)(?: |$)/.exec(label)?.[1]
const sphereStyle = (label: string) => {
  const sphere = sphereOf(label)
  return sphere ? {
    '--sphere-color': `var(--${sphere}, var(--accent))`,
    '--icon': `url('/images/icons/game/${sphere}.svg')`,
  } : undefined
}
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
          :class="{ 'sphere-choice': sphereOf(choice.label) }"
          :style="sphereStyle(choice.label)"
          @click="choice.card && overlay.hide(); emit('choose', choice.index)"
          @pointerenter="(e: PointerEvent) => choice.card && e.pointerType === 'mouse' && overlay.show(choice.card, e.currentTarget as HTMLElement)"
          @pointerleave="(e: PointerEvent) => choice.card && e.pointerType === 'mouse' && overlay.hide()"
        >
          <span v-if="sphereOf(choice.label)" class="icon sphere-icon" aria-hidden="true" />
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
      <RouterLink v-if="campaignId" class="campaign-next" :to="`/campaigns/${campaignId}`">{{ $t('campaignMode.goOn') }}</RouterLink>
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
.campaign-next {
  display: block;
  margin-bottom: 0.75rem;
  padding: 0.5rem 0.75rem;
  border: 1px solid var(--accent);
  border-radius: 6px;
  background: var(--highlight-tint);
  color: inherit;
  font: inherit;
  font-weight: 700;
  text-align: center;
  text-decoration: none;
  cursor: pointer;
}
.campaign-next:hover { background: var(--hover-tint); }
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
.options button.sphere-choice {
  display: flex;
  align-items: center;
  gap: 0.6rem;
  border-color: color-mix(in srgb, var(--sphere-color) 65%, var(--line));
  background: color-mix(in srgb, var(--sphere-color) 22%, var(--panel));
}
.options button.sphere-choice:hover {
  border-color: var(--sphere-color);
  background: color-mix(in srgb, var(--sphere-color) 35%, var(--panel));
}
.sphere-icon { width: 1.2em; height: 1.2em; color: color-mix(in srgb, var(--sphere-color) 70%, var(--text)); }
/* finger-sized answers */
@media (pointer: coarse) {
  .options button { min-height: 44px; font-size: 0.9rem; }
}
.hint { margin: 0; font-size: 0.65rem; opacity: 0.5; font-style: italic; }
</style>
