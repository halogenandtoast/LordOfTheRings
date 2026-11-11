<script setup lang="ts">
import { computed, onBeforeUnmount, ref, watch } from 'vue'
import { useRoute } from 'vue-router'
import { commentOnBug, fetchBug, markBugSeen, type BugDetail } from '../api'
import { refreshBugs, statusKey } from '../bugs'
import { useServerText } from '../i18n'
import PrimaryButton from '../components/PrimaryButton.vue'

const route = useRoute()
const tr = useServerText()
const id = computed(() => route.params.id as string)

const detail = ref<BugDetail | null>(null)
const failed = ref(false)
const reply = ref('')
const sending = ref(false)
const replyFailed = ref<string | null>(null)

// Seeing the report as it is now is what takes a cleared one off the bar.
const seen = async () => {
  await markBugSeen(id.value).catch(() => null)
  void refreshBugs()
}

const load = async () => {
  try {
    detail.value = await fetchBug(id.value)
    failed.value = false
    if (detail.value.report.unread) await seen()
  } catch {
    failed.value = true
  }
}

// opened afresh, and every minute while open, as the server sweeps GitHub
watch(
  id,
  () => {
    detail.value = null
    void load()
  },
  { immediate: true },
)
const timer = setInterval(() => void load(), 60_000)
onBeforeUnmount(() => clearInterval(timer))

const send = async () => {
  if (!reply.value.trim()) return
  sending.value = true
  replyFailed.value = null
  try {
    detail.value = await commentOnBug(id.value, reply.value)
    reply.value = ''
  } catch (error) {
    replyFailed.value = tr(error instanceof Error ? error.message : '') || 'error'
  } finally {
    sending.value = false
  }
}

const status = (value: string) => statusKey(value)
const when = (at: string) => new Date(at).toLocaleString()
</script>

<template>
  <div class="page-container">
    <div class="bug">
      <RouterLink to="/bugs" class="back">← {{ $t('bugs.title') }}</RouterLink>
      <p v-if="failed" class="failed">{{ $t('bugs.loadFailed') }}</p>
      <template v-if="detail">
        <h2 class="title">{{ detail.report.title }}</h2>
        <section class="panel summary">
          <p>
            <span class="label">{{ $t('bugs.status') }}</span>
            <span class="status">{{ $t(status(detail.report.status).key, { column: status(detail.report.status).column }) }}</span>
          </p>
          <p v-if="detail.report.blocking">{{ detail.report.cleared ? $t('bugs.wasBlocking') : $t('bugs.blocking') }}</p>
          <p>
            <RouterLink v-if="detail.gameId" :to="`/games/${detail.gameId}`">{{ $t('bugs.toGame', { name: detail.gameName }) }}</RouterLink>
            <span v-else class="muted">{{ $t('bugs.gameGone') }}</span>
          </p>
          <p>
            <a :href="detail.report.issueUrl" target="_blank" rel="noopener">{{ $t('bugs.onGithub', { number: detail.report.issueNumber }) }}</a>
          </p>
        </section>

        <ol class="timeline">
          <li class="entry event">{{ $t('bugs.filedOn', { date: when(detail.report.createdAt) }) }}</li>
          <li v-for="(entry, i) in detail.entries" :key="i" class="entry" :class="entry.kind === 'status' ? 'event' : ['comment', { mine: entry.mine }]">
            <template v-if="entry.kind === 'status' && entry.status">
              {{ $t('bugs.movedTo', { status: $t(status(entry.status).key, { column: status(entry.status).column }) }) }}
              <time>{{ when(entry.createdAt) }}</time>
            </template>
            <template v-else>
              <header>
                <strong>{{ entry.mine ? $t('bugs.you') : entry.author }}</strong>
                <time>{{ when(entry.createdAt) }}</time>
              </header>
              <p class="body">{{ entry.body }}</p>
            </template>
          </li>
        </ol>

        <form class="panel reply" @submit.prevent="send">
          <label for="bug-reply">{{ $t('bugs.reply') }}</label>
          <textarea id="bug-reply" v-model="reply" rows="4" maxlength="10000" required :placeholder="$t('bugs.replyPlaceholder')"></textarea>
          <p class="muted">{{ $t('bugs.replyPublic') }}</p>
          <p v-if="replyFailed" class="failed">{{ replyFailed === 'error' ? $t('bugs.replyFailed') : replyFailed }}</p>
          <PrimaryButton type="submit" :label="sending ? $t('bugs.sending') : $t('bugs.send')" :disabled="sending" />
        </form>
      </template>
    </div>
  </div>
</template>

<style scoped>
.bug {
  width: min(720px, 100% - 32px);
  margin: 0 auto;
  display: grid;
  gap: 12px;
}
.back { color: var(--accent-bright); font-size: 0.875rem; text-decoration: none; }
.title { margin: 0; color: var(--title); font-size: 1.4rem; overflow-wrap: anywhere; }
.panel {
  display: grid;
  gap: 8px;
  padding: 16px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
}
p { margin: 0; }
a { color: var(--accent-bright); }
.label { color: var(--text-muted); margin-right: 8px; }
.status { color: var(--accent-bright); font-weight: 600; }
.muted { color: var(--text-muted); font-size: 0.85rem; }
.failed { color: var(--danger); }

.timeline { list-style: none; margin: 0; padding: 0; display: grid; gap: 8px; }
.event { font-size: 0.85rem; color: var(--text-muted); padding: 0 4px; }
.event time { margin-left: 8px; font-size: 0.75rem; }
.comment {
  padding: 12px 16px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
}
.comment.mine { border-color: var(--line-strong); }
.comment header { display: flex; flex-wrap: wrap; gap: 4px 10px; align-items: baseline; margin-bottom: 6px; }
.comment strong { color: var(--title); }
.comment time { font-size: 0.75rem; color: var(--text-muted); }
.body { white-space: pre-wrap; overflow-wrap: anywhere; line-height: 1.45; }

.reply label { color: var(--text-muted); font-size: 0.85rem; }
textarea {
  font: inherit;
  color: var(--text);
  background: var(--input-background);
  border: 1px solid var(--box-border);
  border-radius: 3px;
  padding: 8px;
  resize: vertical;
}
textarea:focus { outline: 0; border-color: var(--accent); }
.reply :deep(.primary-btn) { justify-self: end; }
</style>
