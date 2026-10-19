<script setup lang="ts">
/**
 * Renders parsed card text.
 *
 * Recursive over the token tree from `parseCardText`, so nothing reaches
 * `v-html` and the icon tokens become real components.
 */
import { computed } from 'vue'
import GameIcon from './GameIcon.vue'
import { parseCardText, type TextToken } from '@browser/lib/card-text'

const { text, tokens } = defineProps<{
  text?: string | null
  tokens?: TextToken[]
}>()

const parsed = computed(() => tokens ?? parseCardText(text))
</script>

<template>
  <span class="card-text">
    <template v-for="(token, i) in parsed" :key="i">
      <template v-if="token.kind === 'text'">{{ token.value }}</template>
      <br v-else-if="token.kind === 'break'" />
      <GameIcon v-else-if="token.kind === 'icon'" :name="token.name" />
      <b v-else-if="token.kind === 'keyword'" class="keyword">{{ token.value }}</b>
      <b v-else-if="token.style === 'bold'"><CardText :tokens="token.children" /></b>
      <i v-else-if="token.style === 'italic'"><CardText :tokens="token.children" /></i>
      <cite v-else class="cite"><CardText :tokens="token.children" /></cite>
    </template>
  </span>
</template>

<style scoped>
.card-text {
  /* Card text is written with hard line breaks that must survive. */
  white-space: pre-wrap;
  text-wrap: pretty;
}

.keyword {
  color: var(--c-accent);
}

.cite {
  display: block;
  margin-top: 0.15em;
  color: var(--c-text-faint);
  font-style: normal;
  font-size: 0.9em;
}

.cite::before {
  content: '— ';
}
</style>
