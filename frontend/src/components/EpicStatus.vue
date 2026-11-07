<script setup lang="ts">
import { ref, watch } from 'vue'
import { fetchEpicEvent, type EpicEvent } from '../api'
const props = defineProps<{ id: string }>()
const event = ref<EpicEvent | null>(null)
const failed = ref(false)
watch(() => props.id, (id, _, onCleanup) => {
  let active = true
  let timer: ReturnType<typeof setTimeout> | undefined
  event.value = null
  const poll = async () => {
    try { const next = await fetchEpicEvent(id); if (active) { event.value = next; failed.value = false } }
    catch { if (active) failed.value = true }
    finally { if (active) timer = setTimeout(poll, 3000) }
  }
  void poll()
  onCleanup(() => { active = false; clearTimeout(timer) })
}, { immediate: true })
</script>

<template>
  <aside class="epic-status" :aria-label="$t('epic.shared')">
    <RouterLink :to="`/epic-events/${id}`">{{ event?.name ?? $t('epic.title') }} ↗</RouterLink>
    <template v-if="event?.started">
      <span v-if="event.scenario === 'the-siege-of-annuminas'">{{ $t('game.cityStrength') }} <strong>{{ event.cityStrength ?? '—' }}</strong></span>
      <template v-else>
      <span>{{ $t('epic.search') }} <strong>{{ event.progress }}/{{ 8 * (event.totalPlayers ?? 0) }}</strong></span>
      <span>{{ $t('epic.damage') }} <strong>{{ event.damage }}/{{ 15 * (event.totalPlayers ?? 0) }}</strong></span>
      </template>
    </template>
    <span v-else-if="event">{{ $t('epic.organizerStarts') }}</span>
    <span v-if="failed" role="status">{{ $t('epic.reconnecting') }}</span>
  </aside>
</template>

<style scoped>
.epic-status { display: flex; flex-wrap: wrap; align-items: center; gap: .4rem 1.25rem; padding: .5rem 1rem; border-bottom: 1px solid var(--line); background: var(--bar-background); font-size: .8rem; color: var(--text-muted); }
a { color: var(--accent-bright); margin-right: auto; }
a:focus-visible { outline: 2px solid var(--accent); outline-offset: 3px; }
strong { color: var(--text); font-variant-numeric: tabular-nums; margin-left: .3rem; }
</style>
