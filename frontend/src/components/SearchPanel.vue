<script setup lang="ts">
import { computed } from 'vue'
import GameCard from './GameCard.vue'
import type { ChoiceView, QuestionView } from '../types'

const { question } = defineProps<{ question: QuestionView & { searched: NonNullable<QuestionView['searched']> } }>()
const emit = defineEmits<{ choose: [index: number] }>()

const choosable = computed(() => new Set(question.choices.map((c) => c.targetId).filter((id) => id !== null)))
const labels = computed<ChoiceView[]>(() => question.choices.filter((c) => c.targetId === null))
</script>

<template>
  <section class="search-panel" :aria-label="$t('game.search.label')">
    <header>{{ question.prompt }} <span class="count">{{ $t('game.search.searched', { count: question.searched.length }) }}</span></header>
    <div class="cards">
      <GameCard
        v-for="card in question.searched"
        :key="card.id"
        :card="card"
        :choices="question.choices"
        :class="{ unmatched: !choosable.has(card.id) }"
        @choose="emit('choose', $event)"
      />
    </div>
    <div v-if="labels.length" class="options">
      <button v-for="choice in labels" :key="choice.index" type="button" @click="emit('choose', choice.index)">
        {{ choice.label }}
      </button>
    </div>
  </section>
</template>

<style scoped>
.search-panel {
  position: fixed;
  top: 10dvh;
  left: 50%;
  translate: -50% 0;
  z-index: 8000; /* under the card overlay */
  width: min(94vw, 900px);
  padding: 10px;
  border-radius: 10px;
  border: 1px solid var(--line-strong);
  background: rgb(20 16 10 / 0.95);
  backdrop-filter: blur(8px);
  box-shadow: 0 8px 30px var(--shadow-strong);
  color: var(--cream);
}
header { margin: 0 0 8px; font-size: 0.8rem; font-weight: 700; }
.count { font-weight: 400; color: var(--text-muted); }
.cards { display: flex; flex-wrap: wrap; gap: 6px; max-height: 50vh; overflow: auto; --card-width: 70px; }
/* the dimming sits on the image so view transitions keep it */
.cards .unmatched :deep(img) { filter: grayscale(0.85) brightness(0.6); }
.options { display: flex; justify-content: flex-end; margin-top: 8px; }
.options button {
  font: inherit;
  font-size: 0.8rem;
  padding: 0.3rem 0.9rem;
  border: 1px solid var(--line);
  border-radius: 4px;
  background: var(--subtle-tint);
  color: inherit;
  cursor: pointer;
}
.options button:hover { border-color: var(--accent); background: var(--highlight-tint); }
</style>
