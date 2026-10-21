<script lang="ts" setup>
import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRouter } from 'vue-router'
import { createGame } from '../api'
import { currentUser, updateSettings } from '../auth'
import { campaigns, type Campaign, type ScenarioOption } from '../campaigns'
import { useServerText } from '../i18n'

const tr = useServerText()


const { t } = useI18n()
const router = useRouter()
const firstPlayable = (c: Campaign) => c.scenarios.find((s) => s.available) ?? null
const selectedCampaign = ref<Campaign>(campaigns.find(firstPlayable) ?? campaigns[0])
const selectedScenario = ref<ScenarioOption | null>(firstPlayable(selectedCampaign.value))
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

// Seats are taken, each with a deck, once the game is made, where its link brings
// friends to the open ones.
const seatCount = ref(1)

const defaultName = computed(() => selectedScenario.value?.name ?? '')
const canCreate = computed(() => !!selectedScenario.value?.available && !creating.value)

const chooseCampaign = (c: Campaign) => {
  selectedCampaign.value = c
  selectedScenario.value = firstPlayable(c)
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
      decks: [],
      seats: seatCount.value,
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

    <form id="new-game" @submit.prevent="create">
      <fieldset>
        <legend>{{ $t('common.scenario') }}</legend>
        <div class="scenario-row">
          <div class="segmented campaigns" role="radiogroup" :aria-label="$t('newGame.campaign')">
            <button
              v-for="c in campaigns"
              :key="c.id"
              type="button"
              role="radio"
              :aria-checked="c.id === selectedCampaign.id"
              :class="{ selected: c.id === selectedCampaign.id, unavailable: !firstPlayable(c) }"
              @click="chooseCampaign(c)"
            ><span class="icon" :style="{ '--icon': `url(${c.symbol})` }" />{{ c.name }}</button>
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
        </div>
      </fieldset>

      <template v-if="selectedScenario">
        <fieldset>
          <legend>{{ $t('common.game') }}</legend>
          <label>
            {{ $t('newGame.gameName') }}
            <input v-model="gameName" type="text" :placeholder="defaultName" />
          </label>
          <div class="option">
            <span class="option-title">{{ $t('newGame.numberOfPlayers') }}</span>
            <div class="segmented" role="radiogroup">
              <button
                v-for="n in 4"
                :key="n"
                type="button"
                role="radio"
                :aria-checked="seatCount === n"
                :class="{ selected: seatCount === n }"
                @click="seatCount = n"
              >{{ n }}</button>
            </div>
          </div>
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

      <div>
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

.scenario-row { display: grid; grid-template-columns: 220px 1fr; align-items: start; gap: 14px; }
@media (max-width: 600px) {
  .scenario-row { grid-template-columns: 1fr; }
}
.segmented.campaigns { grid-auto-flow: row; }
.segmented.campaigns button { display: flex; align-items: center; gap: 8px; padding: 9px 12px; font-weight: 600; text-align: left; }
.segmented.campaigns button + button { border-left: 0; border-top: 1px solid var(--box-border); }
.campaigns button.unavailable:not(.selected) { color: var(--text-muted); }
.campaigns .icon, .scenario-name .icon { font-size: 1.2em; }

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

.option { display: grid; gap: 8px; }
.option-title { font-size: 0.85rem; color: var(--text-muted); }
.segmented { display: grid; grid-auto-columns: 1fr; grid-auto-flow: column; border: 1px solid var(--box-border); border-radius: 8px; overflow: hidden; }
.segmented button {
  padding: 10px 0;
  border: 0;
  background: var(--input-background);
  color: var(--text);
  font: inherit;
  font-weight: 700;
  cursor: pointer;
}
.segmented button + button { border-left: 1px solid var(--box-border); }
.segmented button:hover:not(.selected) { background: var(--surface-4); }
.segmented button.selected { background: var(--accent); color: var(--button-primary-text); }

.make-default { background: none; border: 0; padding: 0; color: var(--accent); font: inherit; text-decoration: underline; cursor: pointer; }
.saved { color: var(--text-muted); }
.hint { margin: 0; font-size: 0.8rem; }

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
