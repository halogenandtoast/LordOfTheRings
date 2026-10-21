<script setup lang="ts">
// The game log panel, as Arkham's GameLog.vue. It owns what is open, so the live-tail
// rule has one home: the newest entry with detail is open, and shuts when the next
// arrives; a block stays open until something outside it follows. A click pins an entry
// against the rule, so an older attack can be read while play goes on.
import { computed, nextTick, ref, watch } from 'vue'
import GameLogEntry from './GameLogEntry.vue'
import LogGroupBlock from './LogGroupBlock.vue'
import { groupLogEntries, type LogEntry } from '../gameLog'

const props = withDefaults(
  defineProps<{
    entries: readonly LogEntry[]
    /** Whether entries offer "undo back to here": not for a spectator. */
    canUndo?: boolean
    /** Whether there is a box to say something in. */
    canChat?: boolean
    /** Whether older entries are still to be had. */
    hasOlder?: boolean
  }>(),
  { canUndo: false, canChat: false, hasOlder: false },
)

const emit = defineEmits<{
  undo: [step: number, label: string]
  say: [text: string]
  loadOlder: [beforeSeq: number]
}>()

const draft = ref('')
const send = () => {
  const text = draft.value.trim()
  if (!text) return
  draft.value = ''
  emit('say', text)
}

const items = computed(() => groupLogEntries(props.entries))

// The newest top-level entry with detail to show. Indexed into items, as paths are.
const tailPath = computed(() => {
  for (let i = items.value.length - 1; i >= 0; i -= 1) {
    const item = items.value[i]
    if (item.kind === 'entry' && item.entry.children.length > 0) return String(i)
  }
  return null
})

// Kept only where a click differs from the rule, so a pin lapses once they agree.
const pinnedOpen = ref(new Set<string>())
const pinnedShut = ref(new Set<string>())

const autoOpen = (path: string) => {
  const top = path.split('.')[0]
  const item = items.value[Number(top)]
  // Open until something outside it follows, chat aside. Not until its summary: an
  // attack's damage lands after it is decided, and closing then hid it as it arrived.
  if (item?.kind === 'group') {
    return !items.value.slice(Number(top) + 1).some((next) => next.kind === 'group' || next.entry.kind !== 'Chat')
  }
  return tailPath.value !== null && top === tailPath.value
}

const isOpen = (path: string) => (pinnedShut.value.has(path) ? false : pinnedOpen.value.has(path) || autoOpen(path))
const isTail = (path: string) => path === tailPath.value
const isPinned = (path: string) => pinnedOpen.value.has(path) || pinnedShut.value.has(path)

const onToggle = (path: string, open: boolean) => {
  const opened = new Set(pinnedOpen.value)
  const shut = new Set(pinnedShut.value)
  opened.delete(path)
  shut.delete(path)
  if (open !== autoOpen(path)) (open ? opened : shut).add(path)
  pinnedOpen.value = opened
  pinnedShut.value = shut
}

const scroller = ref<HTMLElement | null>(null)
const pinnedToBottom = ref(true)
const loadingOlder = ref(false)

// Follow the log only for a reader already at the bottom; near the top, ask for the
// page before what is held.
const onScroll = () => {
  const el = scroller.value
  if (!el) return
  pinnedToBottom.value = el.scrollHeight - el.scrollTop - el.clientHeight < 40
  const oldest = props.entries[0]?.seq
  if (el.scrollTop < 80 && props.hasOlder && !loadingOlder.value && oldest !== undefined) {
    loadingOlder.value = true
    emit('loadOlder', oldest)
  }
}

// Once older entries land, keep the reader where they were, as the content above them
// just grew.
watch(
  () => props.entries[0]?.seq,
  async () => {
    if (!loadingOlder.value) return
    const el = scroller.value
    const heightBefore = el?.scrollHeight ?? 0
    const topBefore = el?.scrollTop ?? 0
    await nextTick()
    if (el) el.scrollTop = topBefore + el.scrollHeight - heightBefore
    loadingOlder.value = false
  },
)
watch(
  () => props.hasOlder,
  (more) => {
    if (!more) loadingOlder.value = false
  },
)

watch(
  () => props.entries[props.entries.length - 1]?.seq ?? 0,
  async () => {
    if (!pinnedToBottom.value) return
    await nextTick()
    const el = scroller.value
    if (el) el.scrollTop = el.scrollHeight
  },
  { immediate: true, flush: 'post' },
)
</script>

<template>
  <div class="game-log">
    <div ref="scroller" class="game-log__scroll" @scroll.passive="onScroll">
      <ul class="game-log__list">
        <template v-for="(item, i) in items" :key="item.kind === 'group' ? `g${item.id}` : `e${item.entry.seq}`">
          <GameLogEntry
            v-if="item.kind === 'entry'"
            :entry="item.entry"
            :path="String(i)"
            :depth="0"
            :is-open="isOpen"
            :is-tail="isTail"
            :is-pinned="isPinned"
            :can-undo="canUndo"
            @toggle="onToggle"
            @undo="(step, label) => emit('undo', step, label)"
          />
          <LogGroupBlock
            v-else
            :group="item"
            :path="String(i)"
            :is-open="isOpen"
            :is-tail="isTail"
            :is-pinned="isPinned"
            :can-undo="canUndo"
            @toggle="onToggle"
            @undo="(step, label) => emit('undo', step, label)"
          />
        </template>
      </ul>
    </div>

    <form v-if="canChat" class="game-log__compose" @submit.prevent="send">
      <input
        v-model="draft"
        class="game-log__input"
        type="text"
        maxlength="500"
        :placeholder="$t('log.sayPlaceholder')"
        :aria-label="$t('log.sayPlaceholder')"
      />
      <button class="game-log__send" type="submit" :disabled="draft.trim().length === 0">{{ $t('log.say') }}</button>
    </form>
  </div>
</template>

<style scoped>
.game-log {
  flex: 1;
  min-height: 6rem;
  display: flex;
  flex-direction: column;
  background: var(--panel);
  border: 1px solid var(--line);
  border-radius: 6px;
  overflow: hidden;
}
.game-log__scroll {
  /* the pinned banners' heights, which is where each stacks under the last */
  --banner-scenario: 34px;
  --banner-round: 24px;
  --banner-phase: 24px;
  --log-surface: rgb(42 35 25);
  flex: 1 1 auto;
  min-height: 0;
  overflow-y: auto;
  overflow-x: hidden;
  /* none on top: a pinned banner sticks to the edge, not under a gap */
  padding: 0 0.6rem 0.5rem;
  font-size: 0.72rem;
}
.game-log__list { list-style: none; margin: 0; padding: 0; }
.game-log__compose {
  flex: none;
  display: flex;
  gap: 6px;
  padding: 6px;
  border-top: 1px solid var(--line);
  background: rgb(0 0 0 / 0.2);
}
.game-log__input {
  flex: 1 1 auto;
  min-width: 0;
  padding: 5px 8px;
  font: inherit;
  font-size: 0.75rem;
  color: var(--text);
  background: rgb(0 0 0 / 0.3);
  border: 1px solid var(--line);
  border-radius: 4px;
}
.game-log__input::placeholder { color: var(--cream-faint); }
.game-log__input:focus-visible { outline: 2px solid var(--accent); outline-offset: -1px; }
.game-log__send {
  flex: none;
  padding: 5px 10px;
  font: inherit;
  font-size: 0.7rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  text-transform: uppercase;
  color: var(--text-muted);
  background: rgb(229 211 173 / 0.07);
  border: 1px solid var(--line);
  border-radius: 4px;
  cursor: pointer;
}
.game-log__send:hover:not(:disabled) { color: var(--text); background: rgb(229 211 173 / 0.13); }
.game-log__send:disabled { opacity: 0.4; cursor: default; }
</style>
