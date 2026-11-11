<script setup lang="ts">
import { computed, onMounted } from 'vue'
import { bugs, refreshBugs, statusKey } from '../bugs'
import type { BugSummary } from '../api'

onMounted(() => void refreshBugs())

const following = computed(() => bugs.value.filter((bug) => bug.tracked))
const earlier = computed(() => bugs.value.filter((bug) => !bug.tracked))
const when = (at: string) => new Date(at).toLocaleDateString()
const status = (bug: BugSummary) => statusKey(bug.status)
</script>

<template>
  <div class="page-container">
    <div class="bugs">
      <h2 class="page-title">{{ $t('bugs.title') }}</h2>
      <p v-if="!bugs.length" class="empty">{{ $t('bugs.none') }}</p>
      <template v-for="group in [{ key: 'following', list: following }, { key: 'earlier', list: earlier }]" :key="group.key">
        <section v-if="group.list.length" class="group">
          <h3>{{ $t(`bugs.${group.key}`) }}</h3>
          <RouterLink v-for="bug in group.list" :key="bug.id" :to="`/bugs/${bug.id}`" class="bug" :class="{ cleared: bug.cleared }">
            <span class="title">
              {{ bug.title }}
              <span v-if="bug.unread" class="news" :title="$t('bugs.news')" :aria-label="$t('bugs.news')">!</span>
            </span>
            <span class="meta">
              <span class="status">{{ $t(status(bug).key, { column: status(bug).column }) }}</span>
              <span v-if="bug.blocking && !bug.cleared">{{ $t('bugs.blocking') }}</span>
              <span>#{{ bug.issueNumber }}</span>
              <span>{{ $t('bugs.filedOn', { date: when(bug.createdAt) }) }}</span>
            </span>
          </RouterLink>
        </section>
      </template>
    </div>
  </div>
</template>

<style scoped>
.bugs {
  width: min(720px, 100% - 32px);
  margin: 0 auto;
  display: grid;
  gap: 12px;
}
.bugs .page-title { margin: 0; }
.empty { margin: 0; color: var(--text-muted); }
.group { display: grid; gap: 8px; }
h3 { margin: 0; color: var(--title); font-size: 1rem; }
.bug {
  display: grid;
  gap: 4px;
  padding: 12px 16px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  color: var(--text);
  text-decoration: none;
}
.bug:hover { border-color: var(--accent); }
.bug.cleared .title { color: var(--text-muted); }
.title { font-weight: 600; overflow-wrap: anywhere; }
.meta { display: flex; flex-wrap: wrap; gap: 4px 14px; font-size: 0.8rem; color: var(--text-muted); }
.status { color: var(--accent-bright); }
.news {
  display: inline-grid;
  place-items: center;
  width: 1.1rem;
  height: 1.1rem;
  margin-left: 4px;
  border-radius: 50%;
  background: var(--danger);
  color: var(--cream);
  font-size: 0.75rem;
  font-weight: 700;
  vertical-align: middle;
}
</style>
