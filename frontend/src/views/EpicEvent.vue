<script setup lang="ts">
import { computed, ref, watch } from 'vue'
import { RouterLink, useRoute, useRouter } from 'vue-router'
import { createEpicGroup, fetchEpicEvent, startEpicEvent, type EpicDifficulty, type EpicEvent } from '../api'
import { useServerText } from '../i18n'

const route = useRoute()
const router = useRouter()
const tr = useServerText()
const event = ref<EpicEvent | null>(null)
const error = ref<string | null>(null)
const busy = ref(false)
const copied = ref(false)
const name = ref('')
const seats = ref(1)
const difficulty = ref<EpicDifficulty>('NormalMode')
const invite = computed(() => window.location.href)
const isAnnuminas = computed(() => event.value?.scenario === 'the-siege-of-annuminas')
const canAddGroup = computed(() => !isAnnuminas.value || (event.value?.groups.length ?? 0) < 3)
const ready = computed(() => !!event.value?.groups.length && (!isAnnuminas.value || event.value.groups.length === 3) && event.value.groups.every(g => g.joinedSeats === g.seats))
const stageLetter = (ordinal: number) => ['B', 'D', 'F'][ordinal] ?? ''
const totalSeats = computed(() => event.value?.groups.reduce((n, g) => n + g.seats, 0) ?? 0)
const difficultyKey = (mode: EpicDifficulty) => `newGame.mode.${mode === 'EasyMode' ? 'easy' : mode === 'HardMode' ? 'hard' : 'normal'}`
watch(() => String(route.params.id), (id, _, onCleanup) => {
  let active = true
  let timer: ReturnType<typeof setTimeout> | undefined
  event.value = null
  error.value = null
  const poll = async () => {
    try {
      const next = await fetchEpicEvent(id)
      if (active) {
        if (!event.value) difficulty.value = next.difficulty
        event.value = next
        error.value = null
      }
    } catch (e) {
      if (active) error.value = e instanceof Error ? e.message : String(e)
    } finally {
      if (active) timer = setTimeout(poll, 3000)
    }
  }
  void poll()
  onCleanup(() => { active = false; clearTimeout(timer) })
}, { immediate: true })
const addGroup = async () => {
  if (!event.value || busy.value || !name.value.trim() || !canAddGroup.value) return
  busy.value = true
  error.value = null
  try {
    const result = await createEpicGroup(event.value.id, { name: name.value.trim(), seats: seats.value, difficulty: isAnnuminas.value ? 'NormalMode' : difficulty.value })
    await router.push(`/games/${result.gameId}`)
  } catch (e) {
    error.value = e instanceof Error ? e.message : String(e)
  } finally { busy.value = false }
}
const start = async () => {
  if (!event.value || busy.value || !ready.value) return
  busy.value = true
  error.value = null
  try { event.value = await startEpicEvent(event.value.id) }
  catch (e) { error.value = e instanceof Error ? e.message : String(e) }
  finally { busy.value = false }
}
const copy = async () => {
  try { await navigator.clipboard.writeText(invite.value); copied.value = true }
  catch { error.value = '$epic.copyFailed' }
}
</script>

<template>
  <div class="page-container epic-page">
    <main class="epic-content">
      <header class="main-header">
        <div><p class="eyebrow">{{ $t('epic.title') }}</p><h2 class="page-title">{{ event?.name ?? $t('epic.loading') }}</h2></div>
        <RouterLink to="/">{{ $t('home.yourGames') }}</RouterLink>
      </header>
      <p v-if="error" class="error" role="alert">{{ tr(error) }}</p>
      <template v-if="event">
        <p class="intro">{{ $t(isAnnuminas ? 'epic.annuminasDescription' : 'epic.description') }}</p>
        <section v-if="event.started" class="shared" :aria-label="$t('epic.shared')">
          <template v-if="!isAnnuminas">
          <div><span>{{ $t('epic.search') }}</span><strong>{{ event.progress }} / {{ 8 * (event.totalPlayers ?? 0) }}</strong><progress :value="event.progress" :max="8 * (event.totalPlayers ?? 1)" /><small v-if="event.searchComplete">{{ $t('epic.advanceHint') }}</small></div>
          <div><span>{{ $t('epic.damage') }}</span><strong>{{ event.damage }} / {{ 15 * (event.totalPlayers ?? 0) }}</strong><progress :value="event.damage" :max="15 * (event.totalPlayers ?? 1)" /></div>
          </template>
          <div v-else><span>{{ $t('game.cityStrength') }}</span><strong>{{ event.cityStrength ?? '—' }}</strong><small>{{ $t('epic.annuminasCityHint') }}</small></div>
          <p>{{ $t('epic.frozenPlayers', { count: event.totalPlayers ?? 0 }) }}</p>
        </section>
        <section v-else class="invite">
          <label for="epic-invite">{{ $t('epic.invite') }}</label>
          <div class="invite-link"><input id="epic-invite" :value="invite" readonly @focus="($event.target as HTMLInputElement).select()" /><button type="button" @click="copy">{{ $t(copied ? 'game.lobby.copied' : 'game.lobby.copy') }}</button></div>
        </section>
        <section :aria-labelledby="'epic-groups'">
          <div class="section-heading"><h3 id="epic-groups">{{ $t('epic.groups') }}</h3><span>{{ $t('epic.groupSummary', { groups: event.groups.length, players: event.totalPlayers ?? totalSeats }) }}</span></div>
          <div class="groups">
            <component :is="g.canOpen ? RouterLink : 'article'" v-for="g in event.groups" :key="g.id" :to="g.canOpen ? `/games/${g.gameId}` : undefined" class="group-box">
              <span class="group-number">{{ isAnnuminas ? stageLetter(g.ordinal) : g.ordinal + 1 }}</span><div><h4>{{ g.name }}</h4><p><template v-if="isAnnuminas">{{ $t('epic.stage', { stage: stageLetter(g.ordinal) }) }}</template><template v-else>{{ $t(difficultyKey(g.difficulty)) }}</template> · {{ $t('epic.seatsFilled', { joined: g.joinedSeats, seats: g.seats }) }}</p></div><span class="group-action">{{ $t(!g.canOpen ? 'epic.otherGroup' : event.started ? 'epic.openGame' : g.joinedSeats === g.seats ? 'epic.ready' : 'epic.chooseDeck') }}<template v-if="g.canOpen"> →</template></span>
            </component>
            <p v-if="!event.groups.length" class="hint">{{ $t('epic.noGroups') }}</p>
          </div>
        </section>
        <form v-if="!event.started && canAddGroup" class="add-group" @submit.prevent="addGroup">
          <h3>{{ $t('epic.addGroup') }}</h3>
          <div class="group-fields">
            <label>{{ $t('epic.groupName') }}<input v-model="name" required maxlength="120" autocomplete="off" /></label>
            <label>{{ $t('newGame.numberOfPlayers') }}<select v-model="seats"><option v-for="n in 4" :key="n" :value="n">{{ n }}</option></select></label>
            <label v-if="!isAnnuminas">{{ $t('newGame.mode.title') }}<select v-model="difficulty"><option v-for="mode in (['EasyMode', 'NormalMode', 'HardMode'] as const)" :key="mode" :value="mode">{{ $t(difficultyKey(mode)) }}</option></select></label>
          </div>
          <button type="submit" :disabled="busy || !name.trim()">{{ $t('epic.createGroup') }}</button>
        </form>
        <footer v-if="!event.started" class="start-event">
          <p>{{ $t(ready ? 'epic.readyHint' : isAnnuminas ? 'epic.annuminasWaitingHint' : 'epic.waitingHint') }}</p>
          <button v-if="event.canStart" type="button" :disabled="busy || !ready" @click="start">{{ $t('epic.start') }}</button>
          <span v-else>{{ $t('epic.organizerStarts') }}</span>
        </footer>
      </template>
    </main>
  </div>
</template>

<style scoped>
.epic-page { overflow-y: auto; }
.epic-content { width: min(960px, 100%); margin: 0 auto; padding: 1.25rem; display: grid; gap: 1.5rem; }
.main-header, .section-heading, .start-event { display: flex; align-items: center; justify-content: space-between; gap: 1rem; }
.main-header { margin: 0; }
.eyebrow { margin: 0 0 .3rem; color: var(--text-muted); font-size: .8rem; }
h3, h4, p { margin: 0; }
h3 { color: var(--title); font-size: 1.1rem; }
a { color: var(--accent); }
.intro { max-width: 65ch; color: var(--text-muted); line-height: 1.5; }
.invite, .add-group { padding: 1rem; background: var(--box-background); border: 1px solid var(--box-border); border-radius: 4px; display: grid; gap: .8rem; }
label { display: grid; gap: .4rem; font-size: .9rem; }
.invite-link { display: flex; gap: .5rem; }
input, select { width: 100%; min-width: 0; padding: .6rem; color: var(--text); background: var(--input-background); border: 1px solid var(--line); border-radius: 3px; }
button { padding: .6rem 1rem; background: var(--button-primary); color: var(--button-primary-text); border: 1px solid var(--box-border); border-radius: 3px; cursor: pointer; transition: border-color .2s; }
button:hover:not(:disabled), a.group-box:hover { border-color: var(--accent); }
button:disabled { opacity: .5; cursor: default; }
button:focus-visible, a:focus-visible, input:focus-visible, select:focus-visible { outline: 2px solid var(--accent); outline-offset: 3px; }
.section-heading { margin-bottom: .75rem; }
.section-heading > span, .group-box p, .hint, .start-event p, .start-event > span { color: var(--text-muted); font-size: .85rem; }
.groups { display: grid; gap: .6rem; }
.group-box { display: flex; align-items: center; gap: .85rem; padding: .85rem 1rem; border: 1px solid var(--box-border); background: var(--box-background); border-radius: 4px; color: var(--text); text-decoration: none; transition: border-color .2s; }
.group-box h4 { color: var(--title); margin-bottom: .25rem; }
.group-number { font-size: 1.5rem; font-variant-numeric: tabular-nums; color: var(--text-muted); }
.group-action { margin-left: auto; color: var(--accent); font-size: .85rem; }
.group-fields { display: grid; grid-template-columns: 2fr 1fr 1fr; gap: .8rem; }
.add-group > button { justify-self: start; }
.shared { display: grid; grid-template-columns: 1fr 1fr; gap: 1rem; border-bottom: 1px solid var(--line); padding-bottom: 1rem; }
.shared > div { display: grid; grid-template-columns: 1fr auto; gap: .5rem; }
.shared strong { font-variant-numeric: tabular-nums; }
.shared progress, .shared small { grid-column: 1 / -1; width: 100%; accent-color: var(--accent); }
.shared p { grid-column: 1 / -1; color: var(--text-muted); font-size: .85rem; }
.error { color: var(--danger); }
@media (max-width: 600px) { .main-header { align-items: flex-start; flex-direction: column; } .group-fields, .shared { grid-template-columns: 1fr; } .section-heading, .start-event { align-items: flex-start; flex-direction: column; } .group-box { flex-wrap: wrap; } .group-action { width: 100%; margin-left: 2rem; } }
</style>
