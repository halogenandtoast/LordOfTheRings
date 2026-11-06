<script lang="ts" setup>
import { computed, onMounted, ref } from 'vue'
import { slug } from '../campaigns'
import { useRoute, useRouter } from 'vue-router'
import { continueCampaign, extendMirkwoodPaths, fetchCampaign, spendXp, type CampaignView } from '../api'
import { useServerText } from '../i18n'
import type { CardDef } from '../server'
import CardOverlay from '../components/CardOverlay.vue'
import { useCardOverlay } from '../composables/useCardOverlay'
import { refPreview } from '../gameLog'

// The campaign log, as printed on pages 32-33 of the Revised Core rulebook, and the way on
// to the next scenario.
const route = useRoute()
const router = useRouter()
const tr = useServerText()
const id = route.params.id as string
const campaign = ref<CampaignView | null>(null)
const defs = ref<Record<string, CardDef>>({})
const overlay = useCardOverlay()
const previewCard = (code: string, event: Event) => {
  const card = refPreview({ kind: 'RefCard', cardCode: code, cardId: null, entityId: null, name: name(code), faceDown: false })
  if (card) overlay.show(card, event.currentTarget as HTMLElement, false, event instanceof PointerEvent && event.pointerType === 'touch')
}
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
const poolGroups = computed(() => ['boons', 'burdens'].map((key) => {
  const cards = pool.value.filter((p) => p.burden === (key === 'burdens'))
  const rows = new Map<string, { hero: string | null; seat: number | null; cards: typeof cards }>()
  for (const card of cards) {
    const rowKey = card.attachedTo ? `hero-${card.seat ?? 'shared'}-${card.attachedTo}` : card.seat !== null ? `player-${card.seat}` : 'shared'
    const row = rows.get(rowKey) ?? { hero: card.attachedTo, seat: card.seat, cards: [] }
    row.cards.push(card)
    rows.set(rowKey, row)
  }
  return { key, cards, rows: Array.from(rows, ([id, row]) => ({ id, ...row })) }
}))
const campaignTitle = computed(() => {
  const title = campaign.value?.name ?? ''
  return ['The Dark of Mirkwood · 2 scenarios', 'Mirkwood Paths · 3 scenarios', 'Mirkwood Paths · 5 scenarios'].includes(title)
    ? title.replace(/ · \d+ scenarios$/, '')
    : title
})
const current = computed(() => campaign.value?.games[0] ?? null)
const journey = computed(() => (campaign.value?.scenarios ?? []).map((scenario) => ({
  ...scenario,
  score: log.value?.completed.find((entry) => entry.scenario === scenario.id)?.score,
  games: (campaign.value?.games ?? []).filter((game) => game.scenario === scenario.id),
})))
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

const extend = async () => {
  going.value = true
  error.value = null
  try {
    const { id: gameId } = await extendMirkwoodPaths(id)
    await router.push(`/games/${gameId}`)
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
    going.value = false
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
  <div class="page-container campaign-scroll">
    <div class="campaign-page">
    <p v-if="error" class="error">{{ tr(error) }}</p>
    <template v-if="campaign && log">
      <header>
        <h2 class="page-title">{{ campaignTitle }}</h2>
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
        <template v-else-if="campaign.status === 'finished'">
          <p>{{ $t('campaignMode.finished') }}</p>
          <template v-if="campaign.canClaimRangerSense">
            <p class="muted">{{ $t('campaignMode.rangerRewardHint') }}</p>
            <RouterLink class="reward-link" :to="{ path: '/new-game', query: { rewardCampaign: id } }">{{ $t('campaignMode.useRangerReward') }}</RouterLink>
          </template>
          <template v-if="campaign.canExtendMirkwoodPaths">
            <p class="muted">{{ $t('campaignMode.extendHint') }}</p>
            <button class="primary-action" type="button" :disabled="going" @click="extend">{{ $t('campaignMode.extend') }}</button>
          </template>
        </template>
        <RouterLink class="primary-action" v-else-if="current" :to="`/games/${current.id}`">{{ $t('campaignMode.backToGame', { scenario: nextName }) }}</RouterLink>
      </section>

      <section class="card">
        <h3 class="card-title">{{ $t('campaignMode.heroes') }}</h3>
        <p v-if="log.heroes.length === 0" class="muted">{{ $t('campaignMode.notYetDealt') }}</p>
        <ol class="seats">
          <li v-for="(heroes, seat) in log.heroes" :key="seat">
            <span class="muted">{{ $t('campaignMode.player', { n: seat + 1 }) }}</span>
            <ul class="hero-gallery">
              <li v-for="(code, index) in heroes" :key="`${code}-${index}`">
                <button class="card-preview" type="button" :aria-label="name(code)"
                  @pointerenter="(e) => e.pointerType === 'mouse' && previewCard(code, e)"
                  @pointerleave="(e) => e.pointerType === 'mouse' && overlay.hide()"
                  @pointerup="(e) => e.pointerType === 'touch' && previewCard(code, e)"
                  @focus="previewCard(code, $event)" @blur="overlay.hide()" @keydown.esc="overlay.hide()">
                  <img :src="`/images/cards/${code}.avif`" :alt="name(code)" />
                </button>
              </li>
            </ul>
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
        <div class="pool-groups">
        <section v-for="group in poolGroups" :key="group.key" class="pool-group" :class="group.key">
          <h4>{{ $t(`campaignMode.${group.key}`) }} <span class="muted">{{ group.cards.length }}</span></h4>
          <p v-if="!group.cards.length" class="muted">{{ $t('campaignMode.none') }}</p>
        <div v-for="row in group.rows" :key="row.id" class="pool-row">
          <h5 class="pool-owner">
            <template v-if="row.hero">{{ name(row.hero) }}<span v-if="row.seat !== null" class="muted"> · {{ $t('campaignMode.player', { n: row.seat + 1 }) }}</span></template>
            <template v-else-if="row.seat !== null">{{ $t('campaignMode.player', { n: row.seat + 1 }) }}</template>
            <template v-else>{{ $t('campaignMode.shared') }}</template>
          </h5>
        <ul class="pool card-gallery">
          <li v-for="(p, n) in row.cards" :key="n" :class="{ burden: p.burden }">
            <button class="card-preview" type="button" :aria-label="name(p.code)"
              @pointerenter="(e) => e.pointerType === 'mouse' && previewCard(p.code, e)"
              @pointerleave="(e) => e.pointerType === 'mouse' && overlay.hide()"
              @pointerup="(e) => e.pointerType === 'touch' && previewCard(p.code, e)"
              @focus="previewCard(p.code, $event)" @blur="overlay.hide()" @keydown.esc="overlay.hide()">
              <img :src="`/images/cards/${p.code}.avif`" :alt="name(p.code)" />
            </button>

          </li>
        </ul>
        </div>
        </section>
        </div>
      </section>

      <section v-if="Object.keys(log.notes).length || Object.keys(log.counts ?? {}).length" class="card">
        <h3 class="card-title">{{ $t('campaignMode.notes') }}</h3>
        <ul>
          <li v-for="(cards, key) in log.notes" :key="key">{{ $t(`campaignMode.note.${key}`, { cards: cards.map(name).join(', ') }) }}</li>
          <li v-for="(counted, key) in log.counts ?? {}" :key="`count-${key}`">
            {{ $t(`campaignMode.note.${key}`, { cards: Object.entries(counted).map(([code, n]) => `${name(code)} (${n})`).join(', ') }) }}
          </li>
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

      <section class="card journey">
        <div class="section-heading">
          <h3 class="card-title">{{ $t('campaignMode.scenarios') }}</h3>
        </div>
        <ol class="scenario-list">
          <li v-for="(s, index) in journey" :key="s.id" class="scenario" :class="{ 'is-current': s.id === campaign.next }">
            <div class="scenario-marker">
              <span class="scenario-number">{{ String(index + 1).padStart(2, '0') }}</span>
              <span class="scenario-icon" role="img" :aria-label="s.name" :style="{ '--icon': `url('/images/icons/scenarios/${slug(s.name)}.svg')` }" />
            </div>
            <div class="scenario-details">
              <h4>{{ s.name }}</h4>
              <ul v-if="s.games.length" class="game-list">
                <li v-for="(g, attempt) in s.games" :key="g.id">
                  <RouterLink :to="`/games/${g.id}`">
                    {{ $t(g.outcome ? 'campaignMode.viewFinalState' : 'campaignMode.viewCurrentScenario') }}<span v-if="s.games.length > 1"> {{ ` ${s.games.length - attempt}` }}</span>
                    <span aria-hidden="true"> ↗</span>
                  </RouterLink>
                  <span v-if="g.outcome" class="outcome" :class="g.outcome.toLowerCase()">{{ $t(g.outcome === 'Won' ? 'home.won' : 'home.lost') }}</span>
                  <span v-else class="muted">{{ $t('campaignMode.backToGame', { scenario: s.name }) }}</span>
                </li>
              </ul>
            </div>
            <span class="score">{{ s.score ?? '—' }}</span>
          </li>
        </ol>
        <div class="campaign-total"><span>{{ $t('campaignMode.total') }}</span><strong class="total-score">{{ total }}</strong></div>
      </section>
    </template>
    </div>
    <CardOverlay />
  </div>
</template>

<style scoped>
.campaign-scroll { width: 100%; padding-top: 8px; }
.campaign-page { display: grid; align-content: start; gap: 10px; width: min(1120px, 100%); max-width: 100%; margin: 0 auto; color: var(--text); }
@media (max-width: 768px) { .campaign-page { width: 100%; } }
header { padding: 0; margin: 0; display: flex; flex-direction: column; align-items: flex-start; gap: 5px; flex-wrap: wrap; }
.page-title { margin: 0; font-size: clamp(1.8rem, 4vw, 2.6rem); line-height: 1.15; text-wrap: balance; }
.subtitle, .muted { color: var(--text-muted); }
.card { display: grid; gap: 10px; padding: 10px; background: var(--box-background); border: 1px solid var(--box-border); border-radius: 8px; }
.card-title { margin: 0; color: var(--title); font-size: 1.15rem; font-weight: 600; letter-spacing: 0.01em; }
ol, ul { margin: 0; padding-left: 1.2em; }
dl { display: grid; grid-template-columns: max-content 1fr; gap: 4px 16px; margin: 0; }
dt { color: var(--text-muted); }
dd { margin: 0; }
.pool { list-style: none; padding: 0; display: grid; gap: 8px; }
.pool li { display: flex; align-items: center; gap: 10px; }
.pool img { width: 48px; border-radius: 3px; color: transparent; }
.pool .burden strong { color: var(--danger); }
.boxes { margin-left: 8px; letter-spacing: 0.1em; }
table { border-collapse: collapse; width: 100%; }
td { padding: 4px 0; border-bottom: 1px solid var(--line-strong); }
tfoot td { font-weight: 700; border-bottom: 0; }
.score { text-align: right; }
.error { color: var(--danger); }
.reward-link { color: var(--accent); font-weight: 700; }
.primary-action {
  width: fit-content;
  max-width: 100%;
  display: inline-flex;
  align-items: center;
  text-decoration: none;
  font: inherit;
  font-weight: 700;
  padding: 11px 20px;
  background: var(--button-primary);
  color: var(--button-primary-text);
  line-height: 1.4;
  border: 0;
  border-radius: 3px;
  cursor: pointer;
}
.primary-action:disabled { background: var(--button-disabled); color: var(--button-disabled-text); cursor: not-allowed; }

.next { justify-items: start; gap: 10px; }
.next p { margin: 0; }
.seats { list-style: none; padding: 0; display: flex; flex-wrap: wrap; gap: 18px; }
.seats > li { display: grid; gap: 12px; min-width: 0; }
.hero-gallery { display: flex; flex-wrap: wrap; gap: 14px; padding: 0; list-style: none; }
.hero-gallery li { display: grid; gap: 5px; width: clamp(80px, 10vw, 110px); }
.hero-gallery img { width: 100%; border-radius: 7px; }
.hero-gallery strong { font-size: .85rem; line-height: 1.4; }
dl { border-top: 1px solid var(--box-border); padding-top: 10px; font-size: .9rem; }
.card-preview { display: block; width: 100%; padding: 0; border: 0; background: transparent; cursor: zoom-in; border-radius: 4px; }
.card-preview img { display: block; }
.pool-groups { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 18px; }
.pool-group { min-width: 0; }
.pool-row + .pool-row { margin-top: 12px; }
.pool-owner { margin: 0 0 6px; font-size: .8rem; font-weight: 500; }
.pool-group h4 { display: flex; justify-content: space-between; margin: 0 0 10px; padding-bottom: 7px; border-bottom: 1px solid var(--box-border); font-size: .95rem; }
.pool-group p { margin: 0; font-size: .85rem; }
.burdens h4 { color: var(--danger); }
.card-gallery { grid-template-columns: repeat(auto-fill, minmax(80px, 1fr)); gap: 10px; align-items: start; }
.card-gallery li { display: grid; gap: 5px; align-items: start; max-width: 110px; }
.card-gallery img { width: 100%; border-radius: 4px; }
.card-gallery li > span { display: grid; gap: 3px; line-height: 1.35; font-size: .9rem; }
.card-gallery .muted { font-size: .8rem; }
.campaign-total { display: flex; justify-content: space-between; align-items: center; padding-top: 10px; border-top: 1px solid var(--box-border); font-size: .9rem; }
.section-heading { display: flex; justify-content: space-between; align-items: baseline; flex-wrap: wrap; gap: 12px; }
.total-score { margin-left: 12px; color: var(--title); font-variant-numeric: tabular-nums; }
.scenario-list { list-style: none; padding: 0; }
.scenario { display: grid; grid-template-columns: 64px minmax(0, 1fr) auto; gap: 12px; padding: 12px 0; border-top: 1px solid var(--box-border); }
.scenario-marker { display: flex; align-items: center; align-self: start; gap: 10px; }
.scenario-icon { display: block; width: 32px; height: 32px; background: var(--accent); mask: var(--icon) center / contain no-repeat; }
.scenario-number { color: var(--text-muted); font-variant-numeric: tabular-nums; padding-top: 2px; }
.is-current .scenario-number { color: var(--accent); }
.scenario h4 { margin: 0; font-size: 1rem; font-weight: 600; }
.game-list { list-style: none; padding: 0; display: grid; gap: 6px; margin-top: 6px; font-size: .85rem; }
.game-list li { display: flex; gap: 12px; flex-wrap: wrap; align-items: baseline; }
.game-list a { color: var(--accent); text-underline-offset: 4px; }
.outcome { font-size: .8rem; }
.outcome.won { color: var(--success); }
.outcome.lost { color: var(--danger); }
.score { font-variant-numeric: tabular-nums; color: var(--title); }
.primary-action { transition: filter .2s, transform .2s; }
.primary-action:hover { filter: brightness(1.12); }
.primary-action:active { transform: translateY(1px); }
a:focus-visible, button:focus-visible { outline: 2px solid var(--accent); outline-offset: 4px; }
@media (max-width: 480px) {
  .hero-gallery li { width: 80px; }
  .pool-groups { grid-template-columns: 1fr; gap: 16px; }
  .scenario { gap: 10px; grid-template-columns: 64px minmax(0, 1fr) auto; }
  dl { grid-template-columns: 1fr; gap: 6px; }
  dd + dt { margin-top: 8px; }
}
</style>
