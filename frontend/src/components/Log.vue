<script setup lang="ts">
import { nextTick, useTemplateRef, watch } from 'vue'
import { useServerText } from '../i18n'

const { log } = defineProps<{ log: readonly string[] }>()
const tr = useServerText()
const listRef = useTemplateRef<HTMLElement>('listRef')

watch(
  () => log.length,
  async () => {
    await nextTick()
    listRef.value?.scrollTo({ top: listRef.value.scrollHeight })
  },
)
</script>

<template>
  <ol ref="listRef" class="log">
    <li v-for="(line, index) in log" :key="`${index}-${line}`">{{ tr(line) }}</li>
  </ol>
</template>

<style scoped>
.log {
  list-style: none;
  margin: 0;
  padding: 0.5rem 0.6rem;
  background: var(--panel);
  border: 1px solid var(--line);
  border-radius: 6px;
  font-size: 0.72rem;
  line-height: 1.5;
  overflow-y: auto;
  flex: 1;
  min-height: 6rem;
  scroll-behavior: smooth;
}
li { opacity: 0.75; }
li:last-child { opacity: 1; color: var(--accent); }
</style>
