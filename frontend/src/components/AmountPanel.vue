<script setup lang="ts">
import { ref } from 'vue'
import { useServerText } from '../i18n'
import type { QuestionView } from '../types'

const { prompt, amount } = defineProps<{ prompt: string; amount: NonNullable<QuestionView['amount']> }>()
const emit = defineEmits<{ choose: [index: number] }>()
const tr = useServerText()

const value = ref(amount.max)
const confirm = () => emit('choose', Math.min(amount.max, Math.max(amount.min, value.value)) - amount.min)
</script>

<template>
  <section class="amount-panel" role="dialog" :aria-label="tr(prompt)">
    <header>{{ tr(prompt) }}</header>
    <form @submit.prevent="confirm">
      <input v-model.number="value" type="number" :min="amount.min" :max="amount.max" step="1" autofocus />
      <button type="submit">{{ $t('game.amount.confirm') }}</button>
    </form>
  </section>
</template>

<style scoped>
.amount-panel {
  position: fixed;
  top: 20dvh;
  left: 50%;
  translate: -50% 0;
  z-index: 8000; /* under the card overlay */
  width: min(calc(100vw - 32px), 360px);
  padding: 10px;
  border-radius: 10px;
  border: 1px solid var(--line-strong);
  background: rgb(20 16 10 / 0.95);
  backdrop-filter: blur(8px);
  box-shadow: 0 8px 30px var(--shadow-strong);
  color: var(--cream);
}
header { margin: 0 0 8px; font-size: 0.8rem; font-weight: 700; }
form { display: flex; gap: 8px; }
input {
  font: inherit;
  flex: 1;
  min-width: 0;
  padding: 0.3rem 0.5rem;
  border: 1px solid var(--line);
  border-radius: 4px;
  background: var(--subtle-tint);
  color: inherit;
}
button {
  font: inherit;
  font-size: 0.8rem;
  padding: 0.3rem 0.9rem;
  border: 1px solid var(--line);
  border-radius: 4px;
  background: var(--subtle-tint);
  color: inherit;
  cursor: pointer;
}
button:hover { border-color: var(--accent); background: var(--highlight-tint); }
@media (pointer: coarse) {
  input, button { min-height: 44px; }
}
</style>
