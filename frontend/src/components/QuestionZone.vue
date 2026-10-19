<script setup lang="ts">
import { computed } from 'vue'
import { useServerText } from '../i18n'
import type { ChoiceView, QuestionView } from '../types'

const tr = useServerText()

const { question } = defineProps<{ question: QuestionView | null }>()
const emit = defineEmits<{ choose: [index: number] }>()

const labelChoices = computed<ChoiceView[]>(() =>
  (question?.choices ?? []).filter((choice) => choice.targetId === null),
)
const targeted = computed(() => (question?.choices ?? []).length - labelChoices.value.length)
</script>

<template>
  <div class="question" :class="{ idle: !question }">
    <template v-if="question">
      <p class="prompt">{{ tr(question.prompt) }}</p>
      <div class="options">
        <button
          v-for="choice in labelChoices"
          :key="choice.index"
          type="button"
          @click="emit('choose', choice.index)"
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
    <p v-else class="prompt idle-text">{{ $t('game.prompt.waiting') }}</p>
  </div>
</template>

<style scoped>
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
.question.idle { opacity: 0.4; }
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
