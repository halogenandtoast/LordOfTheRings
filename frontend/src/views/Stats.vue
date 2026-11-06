<script lang="ts" setup>
import { computed, ref, watch } from 'vue'
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
  async (m, _, onCleanup) => {
    let stale = false
    onCleanup(() => { stale = true })
    stats.value = null
    error.value = null
    open.value = new Set()
    try {
      const result = await fetchStats(m)
      if (!stale) stats.value = result
    } catch {
      if (!stale) error.value = t('home.serverUnreachable')
    }
  },
  { immediate: true },
)

const key = (row: ScenarioStats) => `${row.scenario}/${row.mode}`
const toggle = (row: ScenarioStats) => {
  const k = key(row)
  if (!open.value.delete(k)) open.value.add(k)
  open.value = new Set(open.value)
}
const modeLabel = { NormalMode: 'newGame.mode.normal', EasyMode: 'newGame.mode.easy', HardMode: 'newGame.mode.hard', NightmareMode: 'newGame.mode.nightmare' }
const totals = computed(() => (stats.value ?? []).reduce((sum, row) => ({
  started: sum.started + row.total.started,
  won: sum.won + row.total.won,
  lost: sum.lost + row.total.lost,
}), { started: 0, won: 0, lost: 0 }))
const winRate = (line: { won: number; lost: number }) =>
  line.won + line.lost ? `${(100 * line.won / (line.won + line.lost)).toFixed(1)}%` : '—'
const spread = (s: Spread | null) => s ? s.average.toFixed(1) : '—'
const range = (s: Spread | null) => s ? `${s.lowest}–${s.highest}` : '—'
</script>

<template>
  <div class="page-container">
    <div class="page-content stats">
      <header class="stats-header">
        <h2 class="page-title">{{ $t('stats.title') }}</h2>
        <p class="intro">{{ $t('stats.intro') }}</p>
      </header>
      <div v-if="currentUser" class="toggle" :aria-label="$t('stats.scope')">
        <button type="button" :class="{ active: !mine }" :aria-pressed="!mine" @click="mine = false">{{ $t('stats.global') }}</button>
        <button type="button" :class="{ active: mine }" :aria-pressed="mine" @click="mine = true">{{ $t('stats.mine') }}</button>
      </div>
      <dl v-if="stats?.length" class="summary">
        <div><dt>{{ $t('stats.started') }}</dt><dd>{{ totals.started }}</dd></div>
        <div><dt>{{ $t('stats.won') }}</dt><dd>{{ totals.won }}</dd></div>
        <div><dt>{{ $t('stats.lost') }}</dt><dd>{{ totals.lost }}</dd></div>
        <div><dt>{{ $t('stats.winRate') }}</dt><dd>{{ winRate(totals) }}</dd></div>
      </dl>
      <div v-if="error" class="box error"><p>{{ error }}</p></div>
      <div v-else-if="stats?.length === 0" class="box"><p>{{ $t('stats.none') }}</p></div>
      <div v-else-if="!stats" class="box" role="status">{{ $t('stats.loading') }}</div>
      <div v-else class="table-scroll"><table>
        <caption>{{ $t('stats.tableHint') }}</caption>
        <thead>
          <tr>
            <th>{{ $t('stats.scenario') }}</th>
            <th>{{ $t('stats.difficulty') }}</th>
            <th>{{ $t('stats.started') }}</th>
            <th>{{ $t('stats.won') }}</th>
            <th>{{ $t('stats.lost') }}</th>
            <th>{{ $t('stats.winRate') }}</th>
            <th>{{ $t('stats.score') }}<small>{{ $t('stats.averageRange') }}</small></th>
            <th>{{ $t('stats.rounds') }}<small>{{ $t('stats.averageRange') }}</small></th>
          </tr>
        </thead>
        <tbody>
          <template v-for="row in stats" :key="key(row)">
            <tr class="scenario">
              <th scope="row"><button class="scenario-toggle" :aria-expanded="open.has(key(row))" @click="toggle(row)"><span aria-hidden="true">{{ open.has(key(row)) ? '▾' : '▸' }}</span> {{ row.name }}</button></th>
              <td>{{ $t(modeLabel[row.mode]) }}</td>
              <td>{{ row.total.started }}</td>
              <td>{{ row.total.won }}</td>
              <td>{{ row.total.lost }}</td>
              <td>{{ winRate(row.total) }}</td>
              <td>{{ spread(row.total.score) }}<small>{{ range(row.total.score) }}</small></td>
              <td>{{ spread(row.total.rounds) }}<small>{{ range(row.total.rounds) }}</small></td>
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
                <td>{{ winRate(deck.stats) }}</td>
                <td>{{ spread(deck.stats.score) }}<small>{{ range(deck.stats.score) }}</small></td>
                <td>{{ spread(deck.stats.rounds) }}<small>{{ range(deck.stats.rounds) }}</small></td>
              </tr>
            </template>
          </template>
        </tbody>
      </table></div>
      <p v-if="stats?.length" class="note">{{ $t('stats.scoreNote') }}</p>
      <p v-if="stats?.length && !mine" class="note">{{ $t('stats.globalNote') }}</p>
    </div>
  </div>
</template>

<style scoped>
.stats { width: 100%; min-width: 0; max-width: 1400px; margin: 0 auto; }
@media (max-width: 768px) { .stats { width: 100%; } }
.box { background: var(--box-background); border: 1px solid var(--box-border); color: var(--text); padding: 14px; border-radius: 5px; }
.box.error { border-color: var(--danger); }
p { margin: 0; }
.toggle { display: flex; gap: 6px; margin-bottom: 12px; }
.toggle button { background: var(--box-background); border: 1px solid var(--box-border); color: var(--text); padding: 4px 12px; border-radius: 4px; cursor: pointer; }
.toggle button.active { border-color: var(--text); font-weight: 600; }
table { width: 100%; border-collapse: collapse; color: var(--text); }
th, td { text-align: left; border-bottom: 1px solid var(--box-border); }
th { font-weight: 600; }
tr.scenario:hover { background: var(--box-background); }
tr.deck td { font-size: 0.9em; }
td { white-space: nowrap; }
.note { margin-top: 8px; font-size: 0.85em; opacity: 0.75; }
.hero { display: inline-block; width: 32px; height: 32px; margin-right: 4px; border-radius: 50%; background-size: 160%; background-position: 50% 20%; }
.stats-header { margin-bottom: 20px; }
.intro { opacity: .75; line-height: 1.6; max-width: 65ch; }
.summary { display: grid; grid-template-columns: repeat(4, 1fr); gap: 20px; padding: 24px 0; margin: 8px 0 24px; border-block: 1px solid var(--box-border); }
.summary dt { font-size: .9rem; opacity: .75; }
.summary dd { margin: 6px 0 0; font-size: 2rem; font-weight: 600; }
.summary, table { font-variant-numeric: tabular-nums; }
.table-scroll { overflow-x: auto; }
caption { text-align: left; padding-bottom: 16px; opacity: .75; font-size: .9rem; }
th, td { padding: 14px 12px; }
small { display: block; margin-top: 4px; font-size: .75rem; font-weight: 400; opacity: .65; }
.scenario-toggle { background: none; border: 0; color: inherit; font: inherit; font-weight: 600; text-align: left; cursor: pointer; padding: 6px 0; }
button:focus-visible { outline: 2px solid var(--text); outline-offset: 4px; }
.toggle button { min-height: 40px; transition: background .2s; }
.deck { background: var(--box-background); }
@media (max-width: 600px) { .summary { grid-template-columns: repeat(2, 1fr); } }
</style>
