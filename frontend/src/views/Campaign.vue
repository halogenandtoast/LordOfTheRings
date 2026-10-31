<script lang="ts" setup>
import { computed, onMounted, ref } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { continueCampaign, fetchCampaign, spendXp, type CampaignView } from '../api'
import { useServerText } from '../i18n'
import type { CardDef } from '../server'

// The campaign log, as printed on pages 32-33 of the Revised Core rulebook, and the way on
// to the next scenario.
const route = useRoute()
const router = useRouter()
const tr = useServerText()
const id = route.params.id as string
const campaign = ref<CampaignView | null>(null)
const defs = ref<Record<string, CardDef>>({})
const error = ref<string | null>(null)
const going = ref(false)

onMounted(async () => {
  try {
    const [view, cards] = await Promise.all([
      fetchCampaign(id),
      fetch('/api/cards').then((r) => (r.ok ? (r.json() as Promise<Record<string, CardDef>>) : {})),
    ])
    campaign.value = view
    defs.value = cards
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  }
})

const name = (code: string) => defs.value[code]?.name ?? code
const isBurden = (code: string) => (defs.value[code]?.cardTraits ?? []).includes('Burden')
const log = computed(() => campaign.value?.log ?? null)
const scenarioName = (sid: string) => campaign.value?.scenarios.find((s) => s.id === sid)?.name ?? sid
const total = computed(() => log.value?.completed.reduce((sum, c) => sum + c.score, 0) ?? 0)
const pool = computed(() => (log.value?.pool ?? []).map((p) => ({ ...p, burden: isBurden(p.code) })))
const current = computed(() => campaign.value?.games[0] ?? null)
const nextName = computed(() => (campaign.value?.next ? scenarioName(campaign.value.next) : ''))

// The Dream-chaser campaign's XP: each card's checkboxes, one marked for each XP spent.
const marked = (code: string) => log.value?.marked[code] ?? 0
const spending = ref(false)
const spend = async (code: string) => {
  if (!campaign.value) return
  spending.value = true
  error.value = null
  try {
    campaign.value = { ...campaign.value, log: await spendXp(id, code) }
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally {
    spending.value = false
  }
}

const goOn = async () => {
  going.value = true
  error.value = null
  try {
    const { id: gameId } = await continueCampaign(id)
    await router.push(`/games/${gameId}`)
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
    going.value = false
  }
}
</script>

<template>
  <div class="page-container campaign-page">
    <p v-if="error" class="error">{{ tr(error) }}</p>
    <template v-if="campaign && log">
      <header>
        <h2 class="page-title">{{ campaign.name }}</h2>
        <span class="subtitle">{{ campaign.campaignName }} &middot; {{ $t('campaignMode.title') }}</span>
      </header>

      <section class="card next">
        <template v-if="campaign.status === 'won'">
          <button class="primary-action" type="button" :disabled="going" @click="goOn">{{ $t('campaignMode.continue', { scenario: nextName }) }}</button>
        </template>
        <template v-else-if="campaign.status === 'lost'">
          <p>{{ $t('campaignMode.lost') }}</p>
          <button class="primary-action" type="button" :disabled="going" @click="goOn">{{ $t('campaignMode.replay', { scenario: nextName }) }}</button>
        </template>
        <p v-else-if="campaign.status === 'finished'">{{ $t('campaignMode.finished') }}</p>
        <RouterLink v-else-if="current" :to="`/games/${current.id}`">{{ $t('campaignMode.backToGame', { scenario: nextName }) }}</RouterLink>
      </section>

      <section class="card">
        <h3 class="card-title">{{ $t('campaignMode.heroes') }}</h3>
        <p v-if="log.heroes.length === 0" class="muted">{{ $t('campaignMode.notYetDealt') }}</p>
        <ol class="seats">
          <li v-for="(heroes, seat) in log.heroes" :key="seat">
            <span class="muted">{{ $t('campaignMode.player', { n: seat + 1 }) }}</span>
            {{ heroes.map(name).join(', ') }}
          </li>
        </ol>
        <dl>
          <dt>{{ $t('campaignMode.fallenHeroes') }}</dt>
          <dd>{{ log.fallenHeroes.length ? log.fallenHeroes.map(name).join(', ') : $t('campaignMode.none') }}</dd>
          <dt>{{ $t('campaignMode.threatPenalty') }}</dt>
          <dd>{{ log.threatPenalty }}</dd>
        </dl>
      </section>

      <section class="card">
        <h3 class="card-title">{{ $t('campaignMode.campaignPool') }}</h3>
        <p v-if="pool.length === 0" class="muted">{{ $t('campaignMode.none') }}</p>
        <ul class="pool">
          <li v-for="(p, n) in pool" :key="n" :class="{ burden: p.burden }">
            <img :src="`/images/cards/thumb/${p.code}.avif`" :alt="name(p.code)" />
            <span>
              <strong>{{ name(p.code) }}</strong>
              <span class="muted"> {{ $t(p.burden ? 'campaignMode.burden' : 'campaignMode.boon') }}</span>
              <span v-if="p.attachedTo" class="muted"> &middot; {{ $t('campaignMode.attachedTo', { hero: name(p.attachedTo) }) }}</span>
              <span v-else-if="p.seat !== null" class="muted"> &middot; {{ $t('campaignMode.player', { n: p.seat + 1 }) }}</span>
            </span>
          </li>
        </ul>
      </section>

      <section v-if="Object.keys(log.notes).length" class="card">
        <h3 class="card-title">{{ $t('campaignMode.notes') }}</h3>
        <ul>
          <li v-for="(cards, key) in log.notes" :key="key">{{ $t(`campaignMode.note.${key}`, { cards: cards.map(name).join(', ') }) }}</li>
        </ul>
      </section>

      <section v-if="campaign.upgrades.length" class="card">
        <h3 class="card-title">{{ $t('campaignMode.experience') }}</h3>
        <p>{{ $t('campaignMode.xpLeft', { xp: log.xp }) }}</p>
        <p v-if="!campaign.canSpendXp" class="muted">{{ $t('campaignMode.spendBetween') }}</p>
        <ul class="pool">
          <li v-for="u in campaign.upgrades" :key="u.card">
            <img :src="`/images/cards/thumb/${marked(u.card) >= u.boxes && u.upgraded ? u.upgraded : u.card}.avif`" :alt="name(u.card)" />
            <span>
              <strong>{{ name(u.card) }}</strong>
              <span class="boxes">{{ '☑'.repeat(Math.min(marked(u.card), u.boxes)) }}{{ '☐'.repeat(Math.max(0, u.boxes - marked(u.card))) }}</span>
            </span>
            <button
              v-if="campaign.canSpendXp && log.xp > 0 && marked(u.card) < u.boxes"
              type="button"
              :disabled="spending"
              @click="spend(u.card)"
            >{{ $t('campaignMode.spendXp') }}</button>
          </li>
        </ul>
      </section>

      <section class="card">
        <h3 class="card-title">{{ $t('campaignMode.scenarios') }}</h3>
        <table>
          <tbody>
            <tr v-for="s in campaign.scenarios" :key="s.id">
              <td>{{ s.name }}</td>
              <td class="score">{{ log.completed.find((c) => c.scenario === s.id)?.score ?? '—' }}</td>
            </tr>
          </tbody>
          <tfoot>
            <tr>
              <td>{{ $t('campaignMode.total') }}</td>
              <td class="score">{{ total }}</td>
            </tr>
          </tfoot>
        </table>
      </section>

      <section class="card">
        <h3 class="card-title">{{ $t('campaignMode.games') }}</h3>
        <ul>
          <li v-for="g in campaign.games" :key="g.id">
            <RouterLink :to="`/games/${g.id}`">{{ scenarioName(g.scenario) }}</RouterLink>
            <span v-if="g.outcome" class="muted"> &middot; {{ $t(g.outcome === 'Won' ? 'home.won' : 'home.lost') }}</span>
          </li>
        </ul>
      </section>
    </template>
  </div>
</template>

<style scoped>
.campaign-page { display: grid; gap: 16px; width: 70vw; max-width: 98vw; margin: 0 auto; color: var(--text); }
@media (max-width: 768px) { .campaign-page { width: 100%; } }
header { display: flex; align-items: baseline; gap: 12px; flex-wrap: wrap; }
.page-title { margin: 0; }
.subtitle, .muted { color: var(--text-muted); }
.card { display: grid; gap: 8px; padding: 16px; background: var(--box-background); border: 1px solid var(--box-border); border-radius: 8px; }
.card-title { margin: 0; color: var(--title); font-size: 0.75rem; font-weight: 700; letter-spacing: 0.1em; text-transform: uppercase; }
ol, ul { margin: 0; padding-left: 1.2em; }
dl { display: grid; grid-template-columns: max-content 1fr; gap: 4px 16px; margin: 0; }
dt { color: var(--text-muted); }
dd { margin: 0; }
.pool { list-style: none; padding: 0; display: grid; gap: 8px; }
.pool li { display: flex; align-items: center; gap: 10px; }
.pool img { width: 48px; border-radius: 3px; }
.pool .burden strong { color: var(--danger); }
.boxes { margin-left: 8px; letter-spacing: 0.1em; }
table { border-collapse: collapse; width: 100%; }
td { padding: 4px 0; border-bottom: 1px solid var(--line-strong); }
tfoot td { font-weight: 700; border-bottom: 0; }
.score { text-align: right; }
.error { color: var(--danger); }
.primary-action {
  width: 100%;
  font: inherit;
  font-weight: 700;
  padding: 15px;
  background: var(--button-primary);
  color: var(--button-primary-text);
  text-transform: uppercase;
  letter-spacing: 0.06em;
  border: 0;
  border-radius: 3px;
  cursor: pointer;
}
.primary-action:disabled { background: var(--button-disabled); color: var(--button-disabled-text); cursor: not-allowed; }
</style>
