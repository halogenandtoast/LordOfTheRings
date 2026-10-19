<script setup lang="ts">
import { computed } from 'vue'
import type { ChoiceView, QuestionView } from '../types'

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
      <p class="prompt">{{ question.prompt }}</p>
      <div class="options">
        <button
          v-for="choice in labelChoices"
          :key="choice.index"
          type="button"
          @click="emit('choose', choice.index)"
        >
          {{ choice.label }}
        </button>
      </div>
      <p v-if="targeted" class="hint">or click a highlighted card</p>
    </template>
    <p v-else class="prompt idle-text">waiting</p>
  </div>
</template>

<style scoped>
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
  background: rgb(255 255 255 / 0.04);
  color: inherit;
  cursor: pointer;
}
.options button:hover { border-color: var(--accent); background: rgb(226 205 145 / 0.12); }
.hint { margin: 0; font-size: 0.65rem; opacity: 0.5; font-style: italic; }
</style>
