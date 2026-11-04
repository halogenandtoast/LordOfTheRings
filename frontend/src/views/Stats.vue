<script lang="ts" setup>
import { ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { currentUser } from '../auth'
import { fetchStats, type ScenarioStats, type Spread } from '../api'

const { t } = useI18n()
const mine = ref(false)
const stats = ref<ScenarioStats[] | null>(null)
const error = ref<string | null>(null)
const open = ref<Set<string>>(new Set())

watch(
  mine,
  (m) => {
    stats.value = null
    fetchStats(m)
      .then((result) => (stats.value = result))
      .catch(() => (error.value = t('home.serverUnreachable')))
  },
  { immediate: true },
)

const key = (row: ScenarioStats) => `${row.scenario}/${row.mode}`
const toggle = (row: ScenarioStats) => {
  const k = key(row)
  if (!open.value.delete(k)) open.value.add(k)
  open.value = new Set(open.value)
}
const modeLabel = { NormalMode: 'newGame.mode.normal', EasyMode: 'newGame.mode.easy', NightmareMode: 'newGame.mode.nightmare' }
// labeled, as one game shows the same number three times
const spread = (s: Spread | null) =>
  s ? `${t('stats.lowest')} ${s.lowest} · ${t('stats.highest')} ${s.highest} · ${t('stats.average')} ${s.average.toFixed(1)}` : '—'
</script>

<template>
  <div class="page-container">
    <div class="page-content stats">
      <h2 class="page-title">{{ $t('stats.title') }}</h2>
      <div v-if="currentUser" class="toggle">
        <button type="button" :class="{ active: !mine }" @click="mine = false">{{ $t('stats.global') }}</button>
        <button type="button" :class="{ active: mine }" @click="mine = true">{{ $t('stats.mine') }}</button>
      </div>
      <div v-if="error" class="box error"><p>{{ error }}</p></div>
      <div v-else-if="stats?.length === 0" class="box"><p>{{ $t('stats.none') }}</p></div>
      <table v-else-if="stats">
        <thead>
          <tr>
            <th>{{ $t('stats.scenario') }}</th>
            <th>{{ $t('stats.difficulty') }}</th>
            <th>{{ $t('stats.started') }}</th>
            <th>{{ $t('stats.won') }}</th>
            <th>{{ $t('stats.lost') }}</th>
            <th>{{ $t('stats.score') }}</th>
            <th>{{ $t('stats.rounds') }}</th>
          </tr>
        </thead>
        <tbody>
          <template v-for="row in stats" :key="key(row)">
            <tr class="scenario" @click="toggle(row)">
              <td>{{ open.has(key(row)) ? '▾' : '▸' }} {{ row.name }}</td>
              <td>{{ $t(modeLabel[row.mode]) }}</td>
              <td>{{ row.total.started }}</td>
              <td>{{ row.total.won }}</td>
              <td>{{ row.total.lost }}</td>
              <td>{{ spread(row.total.score) }}</td>
              <td>{{ spread(row.total.rounds) }}</td>
            </tr>
            <template v-if="open.has(key(row))">
              <tr v-for="deck in row.decks" :key="deck.heroes.join()" class="deck">
                <td colspan="2">
                  <span
                    v-for="hero in deck.heroes"
                    :key="hero"
                    class="hero"
                    :style="{ backgroundImage: `url('/images/cards/${hero}.avif')` }"
                  />
                </td>
                <td>{{ deck.stats.started }}</td>
                <td>{{ deck.stats.won }}</td>
                <td>{{ deck.stats.lost }}</td>
                <td>{{ spread(deck.stats.score) }}</td>
                <td>{{ spread(deck.stats.rounds) }}</td>
              </tr>
            </template>
          </template>
        </tbody>
      </table>
      <p v-if="stats?.length" class="note">{{ $t('stats.scoreNote') }}</p>
      <p v-if="stats?.length && !mine" class="note">{{ $t('stats.globalNote') }}</p>
    </div>
  </div>
</template>

<style scoped>
.stats { width: min(70vw, 100%); margin: 0 auto; }
@media (max-width: 768px) { .stats { width: 100%; } }
.box { background: var(--box-background); border: 1px solid var(--box-border); color: var(--text); padding: 14px; border-radius: 5px; }
.box.error { border-color: var(--danger); }
p { margin: 0; }
.toggle { display: flex; gap: 6px; margin-bottom: 12px; }
.toggle button { background: var(--box-background); border: 1px solid var(--box-border); color: var(--text); padding: 4px 12px; border-radius: 4px; cursor: pointer; }
.toggle button.active { border-color: var(--text); font-weight: 600; }
table { width: 100%; border-collapse: collapse; color: var(--text); }
th, td { padding: 6px 8px; text-align: left; border-bottom: 1px solid var(--box-border); }
th { font-weight: 600; }
tr.scenario { cursor: pointer; }
tr.deck td { font-size: 0.9em; }
td { white-space: nowrap; }
.note { margin-top: 8px; font-size: 0.85em; opacity: 0.75; }
.hero { display: inline-block; width: 32px; height: 32px; margin-right: 4px; border-radius: 50%; background-size: 160%; background-position: 50% 20%; }
</style>
