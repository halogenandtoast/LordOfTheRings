<script setup lang="ts">
import type { QuestionView } from '../types'

defineProps<{ question: QuestionView | null; outcome: 'WON' | 'LOST' | null; round: number; phase: string | null }>()
</script>

<template>
  <aside class="prompt">
    <p v-if="outcome" class="outcome" :class="outcome.toLowerCase()">
      {{ outcome === 'WON' ? 'The players win' : 'The players lose' }}
    </p>
    <p class="round">round {{ round }}<template v-if="phase"> &middot; {{ phase.toLowerCase() }}</template></p>
    <p v-if="question" class="ask">{{ question.prompt }}</p>
  </aside>
</template>

<style scoped>
.prompt {
  background: var(--panel);
  border: 1px solid var(--line);
  border-radius: 6px;
  padding: 0.6rem 0.75rem;
  display: flex;
  flex-direction: column;
  gap: 0.35rem;
}
.round { margin: 0; font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.08em; opacity: 0.55; }
.ask { margin: 0; font-size: 0.9rem; }
.outcome { margin: 0; font-weight: 700; }
.outcome.won { color: var(--accent); }
.outcome.lost { color: var(--danger); }
</style>
