<script lang="ts" setup>
import { computed, onMounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRouter } from 'vue-router'
import { createGame } from '../api'
import DeckPicker from '../components/DeckPicker.vue'
import DeckTile from '../components/DeckTile.vue'
import { deckFaces, toDeckChoice, type DeckKey } from '../deckChoice'
import { fetchDecks } from '../browser/lib/api'
import type { DeckSummary } from '../browser/types/deck'
import { currentUser, updateSettings } from '../auth'
import { campaigns, type Campaign, type ScenarioOption } from '../campaigns'
import { useServerText } from '../i18n'

const tr = useServerText()


const { t } = useI18n()
const router = useRouter()
const selectedCampaign = ref<Campaign | null>(null)
const selectedScenario = ref<ScenarioOption | null>(null)
const gameName = ref('')
const creating = ref(false)
// As in Arkham: decided when the game is made, starting from the player's default,
// which they can make whatever they picked here.
const allowSpectators = ref(currentUser.value?.allowSpectatorsByDefault === true)
const differsFromDefault = computed(() => allowSpectators.value !== (currentUser.value?.allowSpectatorsByDefault === true))
const savedDefault = ref(false)
watch(allowSpectators, () => (savedDefault.value = false))
const makeDefault = async () => {
  await updateSettings({ allowSpectatorsByDefault: allowSpectators.value })
  savedDefault.value = true
}
const easyMode = ref(false)
const error = ref<string | null>(null)

const decks = ref<DeckSummary[]>([])
// solo plays one deck; multi-handed plays several, all yours; with friends you bring the
// first deck and friends join with theirs
type Mode = 'solo' | 'multi' | 'friends'
const modes: { id: Mode; icon: string }[] = [
  { id: 'solo', icon: 'M15.75 6a3.75 3.75 0 1 1-7.5 0 3.75 3.75 0 0 1 7.5 0ZM4.501 20.118a7.5 7.5 0 0 1 14.998 0A17.933 17.933 0 0 1 12 21.75c-2.676 0-5.216-.584-7.499-1.632Z' },
  { id: 'multi', icon: 'M16.5 8.25V6a2.25 2.25 0 0 0-2.25-2.25H6A2.25 2.25 0 0 0 3.75 6v8.25A2.25 2.25 0 0 0 6 16.5h2.25m8.25-8.25H18a2.25 2.25 0 0 1 2.25 2.25V18A2.25 2.25 0 0 1 18 20.25h-7.5A2.25 2.25 0 0 1 8.25 18v-1.5m8.25-8.25h-6a2.25 2.25 0 0 0-2.25 2.25v6' },
  { id: 'friends', icon: 'M18 18.72a9.094 9.094 0 0 0 3.741-.479 3 3 0 0 0-4.682-2.72m.94 3.198.001.031c0 .225-.012.447-.037.666A11.944 11.944 0 0 1 12 21c-2.17 0-4.207-.576-5.963-1.584A6.062 6.062 0 0 1 6 18.719m12 0a5.971 5.971 0 0 0-.941-3.197m0 0A5.995 5.995 0 0 0 12 12.75a5.995 5.995 0 0 0-5.058 2.772m0 0a3 3 0 0 0-4.681 2.72 8.986 8.986 0 0 0 3.74.477m.94-3.197a5.971 5.971 0 0 0-.94 3.197M15 6.75a3 3 0 1 1-6 0 3 3 0 0 1 6 0Zm6 3a2.25 2.25 0 1 1-4.5 0 2.25 2.25 0 0 1 4.5 0Zm-13.5 0a2.25 2.25 0 1 1-4.5 0 2.25 2.25 0 0 1 4.5 0Z' },
]
const mode = ref<Mode>('solo')
const seatCount = ref(2)
const deckChoices = ref<DeckKey[]>(['starter:leadership', 'starter:tactics', 'starter:spirit', 'starter:lore'])
// the seat whose deck the picker is choosing; only multi-handed has more than yours
const chosenSeat = ref(0)
const pickingSeat = computed(() => (mode.value === 'multi' ? Math.min(chosenSeat.value, seatCount.value - 1) : 0))
const faces = computed(() => new Map(deckFaces(decks.value).map((face) => [face.key, face])))
const faceOf = (key: DeckKey) => faces.value.get(key) ?? deckFaces([])[0]
onMounted(async () => {
  try {
    decks.value = await fetchDecks()
  } catch {
    // the starter decks are enough to play with
  }
})

const playableCount = (c: Campaign) => c.scenarios.filter((s) => s.available).length
const defaultName = computed(() => selectedScenario.value?.name ?? '')
const canCreate = computed(() => !!selectedScenario.value?.available && !creating.value)

const chooseCampaign = (c: Campaign) => {
  selectedCampaign.value = c
  selectedScenario.value = c.scenarios.find((s) => s.available) ?? null
}

const back = () => {
  selectedCampaign.value = null
  selectedScenario.value = null
  error.value = null
}

const cancel = () => router.push('/')

const create = async () => {
  if (!canCreate.value) return
  creating.value = true
  error.value = null
  try {
    const { id } = await createGame({
      name: gameName.value.trim() || defaultName.value,
      scenario: selectedScenario.value?.id,
      decks: deckChoices.value.slice(0, mode.value === 'multi' ? seatCount.value : 1).map(toDeckChoice),
      withFriends: mode.value === 'friends',
      seats: mode.value === 'friends' ? seatCount.value : undefined,
      allowSpectators: allowSpectators.value,
      easyMode: easyMode.value,
    })
    await router.push(`/games/${id}`)
  } catch (e) {
    error.value = e instanceof Error ? e.message : t('newGame.couldNotCreateGame')
    creating.value = false
  }
}
</script>

<template>
  <div class="new-game-content">
    <header class="main-header">
      <h2>{{ $t('newGame.title') }}</h2>
      <button type="button" class="cancel-new-game-button" @click="cancel">{{ $t('common.cancel') }}</button>
    </header>

    <div v-if="!selectedCampaign" id="new-game">
      <fieldset>
        <legend>{{ $t('newGame.campaign') }}</legend>
        <div class="campaigns">
          <button
            v-for="c in campaigns"
            :key="c.id"
            type="button"
            class="campaign-box"
            :class="{ unavailable: playableCount(c) === 0 }"
            @click="chooseCampaign(c)"
          >
            <img :src="c.image" alt="" />
            <span class="campaign-name"><span class="icon" :style="{ '--icon': `url(${c.symbol})` }" />{{ c.name }}</span>
            <span class="campaign-count">
              {{ $t('newGame.scenarioCount', c.scenarios.length) }}<template v-if="playableCount(c) > 0"> &middot; {{ $t('newGame.playableCount', { count: playableCount(c) }) }}</template>
            </span>
          </button>
        </div>
      </fieldset>
    </div>

    <form v-else id="new-game" @submit.prevent="create">
      <fieldset>
        <legend>{{ $t('common.scenario') }}</legend>
        <div class="campaign-header">
          <img :src="selectedCampaign.image" alt="" />
          <div>
            <h3><span class="icon" :style="{ '--icon': `url(${selectedCampaign.symbol})` }" /> {{ selectedCampaign.name }}</h3>
            <button type="button" class="change-campaign" @click="back">{{ $t('newGame.chooseDifferentCampaign') }}</button>
          </div>
        </div>
        <div class="scenarios">
          <button
            v-for="option in selectedCampaign.scenarios"
            :key="option.id"
            type="button"
            class="scenario-box"
            :class="{ selected: option.id === selectedScenario?.id, unavailable: !option.available }"
            :disabled="!option.available"
            :aria-pressed="option.id === selectedScenario?.id"
            @click="selectedScenario = option"
          >
            <img :src="option.image" alt="" />
            <span class="scenario-name"><span class="icon" :style="{ '--icon': `url(${option.icon})` }" />{{ option.name }}</span>
            <span v-if="!option.available" class="coming-soon">{{ $t('newGame.comingSoon') }}</span>
          </button>
        </div>
      </fieldset>

      <template v-if="selectedScenario">
        <fieldset>
          <legend>{{ $t('newGame.whoPlays') }}</legend>
          <div class="modes" role="radiogroup">
            <button
              v-for="m in modes"
              :key="m.id"
              type="button"
              role="radio"
              class="mode"
              :class="{ selected: mode === m.id }"
              :aria-checked="mode === m.id"
              @click="mode = m.id"
            >
              <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
                <path stroke-linecap="round" stroke-linejoin="round" :d="m.icon" />
              </svg>
              <strong>{{ $t(`newGame.modes.${m.id}.title`) }}</strong>
              <span>{{ $t(`newGame.modes.${m.id}.hint`) }}</span>
            </button>
          </div>
          <div v-if="mode !== 'solo'" class="seat-count">
            {{ $t(mode === 'friends' ? 'newGame.howManyPlayers' : 'newGame.howManyDecks') }}
            <div class="segmented" role="radiogroup">
              <button
                v-for="n in [2, 3, 4]"
                :key="n"
                type="button"
                role="radio"
                :aria-checked="seatCount === n"
                :class="{ selected: seatCount === n }"
                @click="seatCount = n"
              >{{ n }}</button>
            </div>
          </div>
        </fieldset>

        <fieldset>
          <legend>{{ $t(mode === 'multi' ? 'newGame.yourDecks' : 'newGame.yourDeck') }}</legend>
          <div v-if="mode !== 'solo'" class="seats">
            <template v-if="mode === 'multi'">
              <button
                v-for="seat in seatCount"
                :key="seat"
                type="button"
                class="seat"
                :class="{ picking: pickingSeat === seat - 1 }"
                @click="chosenSeat = seat - 1"
              >
                <span class="seat-label">{{ $t('newGame.seatDeck', { seat }) }}</span>
                <DeckTile :face="faceOf(deckChoices[seat - 1])" :selected="pickingSeat === seat - 1" />
              </button>
            </template>
            <template v-else>
              <div class="seat picking">
                <span class="seat-label">{{ currentUser?.username }}</span>
                <DeckTile :face="faceOf(deckChoices[0])" selected />
              </div>
              <div v-for="n in seatCount - 1" :key="n" class="seat">
                <span class="seat-label">{{ $t('newGame.openSeat') }}</span>
                <span class="open-seat">{{ $t('newGame.friendBringsDeck') }}</span>
              </div>
            </template>
          </div>
          <p v-if="mode === 'multi'" class="hint">{{ $t('newGame.pickingFor', { seat: pickingSeat + 1 }) }}</p>
          <DeckPicker v-model="deckChoices[pickingSeat]" :decks="decks" />
        </fieldset>

        <fieldset>
          <legend>{{ $t('common.game') }}</legend>
          <label>
            {{ $t('newGame.gameName') }}
            <input v-model="gameName" type="text" :placeholder="defaultName" />
          </label>
          <div class="toggle-row">
            <label class="switch">
              <input v-model="easyMode" type="checkbox" role="switch" />
              <span class="track" aria-hidden="true" />
              {{ $t('newGame.easyMode') }}
            </label>
            <p class="hint">{{ $t('newGame.easyModeHint') }}</p>
          </div>
          <div class="toggle-row">
            <label class="switch">
              <input v-model="allowSpectators" type="checkbox" role="switch" />
              <span class="track" aria-hidden="true" />
              {{ $t('newGame.allowSpectators') }}
            </label>
            <button v-if="differsFromDefault" type="button" class="make-default" @click="makeDefault">{{ $t('newGame.makeDefault') }}</button>
            <span v-else-if="savedDefault" class="saved">{{ $t('newGame.defaultSaved') }}</span>
            <p class="hint">{{ $t('newGame.allowSpectatorsHint') }}</p>
          </div>
        </fieldset>
      </template>

      <p v-if="error" class="error">{{ tr(error) }}</p>

      <div class="wizard-actions">
        <button type="button" class="secondary-action" @click="back">{{ $t('common.back') }}</button>
        <button class="primary-action" type="submit" :disabled="!canCreate">
          {{ creating ? $t('newGame.creating') : $t('common.create') }}
        </button>
      </div>
    </form>
  </div>
</template>

<style scoped>
.new-game-content {
  flex: 1;
  min-height: 0;
  overflow-y: auto;
  scrollbar-gutter: stable;
  padding: 20px 10px 10px;
}

header,
#new-game {
  width: 70vw;
  max-width: 98vw;
  min-width: 60vw;
  margin: 0 auto;
}

@media (max-width: 768px) {
  header, #new-game { min-width: unset; width: 100%; }
}

header {
  display: flex;
  align-items: center;
  gap: 12px;
  margin-bottom: 10px;
}

h2 {
  color: var(--title);
  text-transform: uppercase;
  letter-spacing: 0.04em;
  font-size: 2em;
  margin: 0 auto 0 0;
}

.cancel-new-game-button {
  background-color: var(--button-secondary);
  border: 0;
  border-radius: 3px;
  color: var(--text);
  font: inherit;
  font-weight: bolder;
  padding: 10px 15px;
  text-transform: uppercase;
  cursor: pointer;
  transition: background-color 0.3s linear;
}
.cancel-new-game-button:hover { background-color: var(--button-secondary-hover); }

#new-game {
  display: grid;
  gap: 14px;
  color: var(--text);
}

fieldset {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 5px;
  padding: 12px 14px 14px;
  margin: 0;
  display: grid;
  gap: 10px;
}

legend {
  color: var(--title);
  font-size: 0.75rem;
  font-weight: 700;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  padding: 0 6px;
}

.campaigns {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(170px, 1fr));
  gap: 12px;
}

.campaign-box {
  padding: 0;
  border: 2px solid transparent;
  border-radius: 5px;
  overflow: hidden;
  background: var(--input-background);
  cursor: pointer;
  display: flex;
  flex-direction: column;
  color: var(--text);
  font: inherit;
  text-align: center;
  transition: border-color 0.15s, transform 0.15s;
}
.campaign-box img { width: 100%; aspect-ratio: 1; object-fit: cover; display: block; }
.campaign-box:hover { border-color: var(--accent); transform: translateY(-2px); }
.campaign-box.unavailable img { filter: grayscale(70%); }
.campaign-name { display: flex; align-items: center; gap: 6px; padding: 8px 8px 0; font-weight: 700; text-align: left; }
.campaign-name .icon, .scenario-name .icon { font-size: 1.2em; }
.campaign-count { padding: 2px 8px 8px; font-size: 0.75em; color: var(--text-muted); text-transform: uppercase; letter-spacing: 0.04em; }

.campaign-header {
  display: flex;
  align-items: center;
  gap: 14px;
}
.campaign-header img { width: 84px; border-radius: 4px; box-shadow: 0 2px 8px var(--shadow); }
.campaign-header h3 { margin: 0 0 4px; color: var(--title); font-size: 1.4em; }
.change-campaign {
  background: none;
  border: 0;
  padding: 0;
  color: var(--accent-bright);
  font: inherit;
  font-size: 0.85em;
  text-decoration: underline;
  cursor: pointer;
}

.scenarios {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
  gap: 10px;
}

.scenario-box {
  position: relative;
  padding: 0;
  border: 2px solid transparent;
  border-radius: 5px;
  overflow: hidden;
  background: var(--input-background);
  cursor: pointer;
  display: flex;
  flex-direction: column;
  color: var(--text);
  font: inherit;
}
.scenario-box img { width: 100%; aspect-ratio: 1; object-fit: cover; display: block; }
.scenario-box:not(.selected) img { filter: grayscale(100%); }
.scenario-box.selected { border-color: var(--accent); }
.scenario-box.unavailable { cursor: not-allowed; opacity: 0.6; }
.scenario-name { display: flex; align-items: center; justify-content: center; gap: 6px; padding: 8px; font-weight: 700; text-align: center; }
.coming-soon {
  position: absolute;
  top: 8px;
  right: 8px;
  background: var(--panel);
  color: var(--text-muted);
  font-size: 0.7em;
  font-weight: 700;
  text-transform: uppercase;
  padding: 2px 6px;
  border-radius: 3px;
}



label {
  display: grid;
  gap: 4px;
  font-size: 0.85rem;
  color: var(--text-muted);
}

input[type='text'], select {
  font: inherit;
  font-size: 1rem;
  color: var(--text);
  outline: 0;
  border: 1px solid var(--box-border);
  border-radius: 3px;
  padding: 12px;
  background: var(--input-background);
}
/* the native arrow hugs the edge, so draw one that can sit in from it */
select {
  appearance: none;
  padding-right: 40px;
  background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='%23c2b79b' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='m6 9 6 6 6-6'/%3E%3C/svg%3E");
  background-repeat: no-repeat;
  background-position: right 14px center;
  background-size: 16px;
}
input[type='text']:focus, select:focus { border-color: var(--accent); }
input::placeholder { color: var(--text-muted); opacity: 0.7; }

.deck { margin: 0; color: var(--text-muted); font-size: 0.85rem; }
.deck strong { color: var(--text); }

.error { margin: 0; color: var(--danger); }

.toggle-row { display: flex; flex-wrap: wrap; align-items: center; gap: 6px 16px; font-size: 0.85rem; color: var(--text-muted); }
.toggle-row .hint { flex-basis: 100%; }
.switch { display: flex; align-items: center; gap: 10px; color: var(--text); font-size: 0.95rem; cursor: pointer; }
.switch input { position: absolute; opacity: 0; pointer-events: none; }
.track {
  position: relative;
  width: 38px;
  height: 22px;
  border-radius: 11px;
  background: var(--surface-4);
  transition: background 0.15s;
}
.track::after {
  content: '';
  position: absolute;
  top: 3px;
  left: 3px;
  width: 16px;
  height: 16px;
  border-radius: 50%;
  background: var(--cream);
  transition: translate 0.15s;
}
.switch input:checked + .track { background: var(--accent); }
.switch input:checked + .track::after { translate: 16px 0; }
.switch input:focus-visible + .track { outline: 2px solid var(--accent-bright); outline-offset: 2px; }

.modes { display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 10px; }
.mode {
  display: grid;
  grid-template-columns: auto 1fr;
  grid-template-rows: auto auto;
  column-gap: 12px;
  align-items: center;
  padding: 14px;
  border: 2px solid transparent;
  border-radius: 6px;
  background: var(--input-background);
  color: var(--text);
  font: inherit;
  text-align: left;
  cursor: pointer;
  transition: border-color 0.15s;
}
.mode svg { grid-row: span 2; width: 32px; height: 32px; color: var(--text-muted); }
.mode span { font-size: 0.8rem; color: var(--text-muted); }
.mode:hover { border-color: var(--line-strong); }
.mode.selected { border-color: var(--accent); }
.mode.selected svg { color: var(--accent-bright); }

.seat-count { display: flex; align-items: center; gap: 12px; font-size: 0.9rem; color: var(--text-muted); }
.segmented { display: inline-flex; border: 1px solid var(--box-border); border-radius: 5px; overflow: hidden; }
.segmented button {
  min-width: 44px;
  padding: 8px 0;
  border: 0;
  background: var(--input-background);
  color: var(--text);
  font: inherit;
  font-weight: 700;
  cursor: pointer;
}
.segmented button + button { border-left: 1px solid var(--box-border); }
.segmented button.selected { background: var(--accent); color: var(--button-primary-text); }

.seats { display: grid; grid-template-columns: repeat(auto-fill, minmax(150px, 1fr)); gap: 10px; padding-bottom: 12px; border-bottom: 1px solid var(--line); }
.seat { display: grid; gap: 4px; align-content: start; padding: 0; border: 0; background: none; font: inherit; color: inherit; text-align: left; }
button.seat { cursor: pointer; }
.seat-label { font-size: 0.7rem; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase; color: var(--text-muted); }
.seat.picking .seat-label { color: var(--accent-bright); }
.open-seat {
  display: grid;
  place-items: center;
  aspect-ratio: 150 / 118;
  padding: 10px;
  border: 2px dashed var(--line);
  border-radius: 6px;
  color: var(--text-muted);
  font-size: 0.8rem;
  text-align: center;
}
.make-default { background: none; border: 0; padding: 0; color: var(--accent); font: inherit; text-decoration: underline; cursor: pointer; }
.saved { color: var(--text-muted); }
.hint { margin: 0; font-size: 0.8rem; }

.wizard-actions {
  display: grid;
  grid-template-columns: 1fr 3fr;
  gap: 10px;
}

.secondary-action {
  font: inherit;
  font-weight: 700;
  padding: 15px;
  background: var(--button-secondary);
  color: var(--text);
  text-transform: uppercase;
  letter-spacing: 0.06em;
  border: 0;
  border-radius: 3px;
  cursor: pointer;
}
.secondary-action:hover { background: var(--button-secondary-hover); }

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
.primary-action:hover { background: var(--button-primary-hover); }
.primary-action:disabled { background: var(--button-disabled); color: var(--button-disabled-text); cursor: not-allowed; }
</style>
