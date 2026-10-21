<script lang="ts" setup>
import { computed, nextTick, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRoute, useRouter } from 'vue-router'
import { createGame } from '../api'
import { currentUser, updateSettings } from '../auth'
import { campaigns as allCampaigns, type Campaign, type ScenarioOption } from '../campaigns'
import { alphaEnabled, filterDisplayable, isDevBuild } from '../displayRules'
import { useServerText } from '../i18n'
import { useShortcuts } from '../browser/lib/keys'

const tr = useServerText()


const { t } = useI18n()
const router = useRouter()
const displayRules = {
  alpha: alphaEnabled(useRoute().query),
  beta: currentUser.value?.beta === true,
  dev: isDevBuild(),
}
// the campaigns with something to show, and only what they have to show
const campaigns = allCampaigns
  .map((c) => ({ ...c, scenarios: filterDisplayable(c.scenarios, displayRules) }))
  .filter((c) => c.scenarios.length > 0)
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

// Seats are taken, each with a deck, once the game is made, where its link brings
// friends to the open ones.
const seatCount = ref(1)

const playableCount = (c: Campaign) => c.scenarios.filter((s) => !s.unimplemented).length
const defaultName = computed(() => selectedScenario.value?.name ?? '')
const canCreate = computed(() => !!selectedScenario.value && !selectedScenario.value.unimplemented && !creating.value)

// As in Arkham, each step is a view transition: every box is named for what it shows,
// so the one chosen moves into the panel on the left, and back out again. The new
// state is captured once Vue has drawn it.
const step = (change: () => void) => {
  if (!document.startViewTransition) return change()
  document.startViewTransition(async () => {
    change()
    await nextTick()
  })
}

const chooseCampaign = (c: Campaign) =>
  step(() => {
    selectedCampaign.value = c
    selectedScenario.value = null
  })

const chooseScenario = (s: ScenarioOption) => step(() => (selectedScenario.value = s))

// a step back: from the game's options to the scenarios, from there to the campaigns
const back = () =>
  step(() => {
    if (selectedScenario.value) selectedScenario.value = null
    else selectedCampaign.value = null
    error.value = null
  })
useShortcuts([{ key: 'Escape', description: 'Go back a step.', run: () => selectedCampaign.value && back() }])

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
      <button v-if="selectedCampaign" type="button" class="back-button" @click="back">{{ $t('common.back') }}</button>
      <button type="button" class="cancel-new-game-button" @click="cancel">{{ $t('common.cancel') }}</button>
    </header>

    <div v-if="!selectedCampaign" id="new-game">
      <section class="card">
        <h3 class="card-title">{{ $t('newGame.campaign') }}</h3>
        <div class="campaigns">
          <button
            v-for="c in campaigns"
            :key="c.id"
            type="button"
            class="campaign-box"
            :class="{ unavailable: playableCount(c) === 0 }"
            @click="chooseCampaign(c)"
          >
            <img :src="c.image" alt="" :style="{ viewTransitionName: `campaign-${c.id}` }" />
            <span class="campaign-name"><span class="icon" :style="{ '--icon': `url(${c.symbol})` }" />{{ c.name }}</span>
            <span class="campaign-count">
              {{ $t('newGame.scenarioCount', c.scenarios.length) }}<template v-if="playableCount(c) < c.scenarios.length"> &middot; {{ $t('newGame.playableCount', { count: playableCount(c) }) }}</template>
            </span>
          </button>
        </div>
      </section>
    </div>

    <form v-else id="new-game" @submit.prevent="create">
      <section v-if="!selectedScenario" class="card split">
        <div class="panel">
          <h3 class="card-title">{{ $t('newGame.campaign') }}</h3>
          <img :src="selectedCampaign.image" alt="" :style="{ viewTransitionName: `campaign-${selectedCampaign.id}` }" />
          <h3 class="panel-title"><span class="icon" :style="{ '--icon': `url(${selectedCampaign.symbol})` }" />{{ selectedCampaign.name }}</h3>
        </div>
        <div class="card-body">
          <h3 class="card-title">{{ $t('common.scenario') }}</h3>
          <div class="scenarios">
            <button
              v-for="option in selectedCampaign.scenarios"
              :key="option.id"
              type="button"
              class="scenario-box"
              :class="{ unavailable: option.unimplemented, alpha: option.alpha, beta: option.beta }"
              :disabled="option.unimplemented"
              @click="chooseScenario(option)"
            >
              <img :src="option.image" alt="" :style="{ viewTransitionName: `scenario-${option.id}` }" />
              <span class="scenario-name"><span class="icon" :style="{ '--icon': `url(${option.icon})` }" />{{ option.name }}</span>
            </button>
          </div>
        </div>
      </section>

      <template v-else>
        <section class="card split">
          <div class="panel">
            <h3 class="card-title">{{ $t('common.scenario') }}</h3>
            <img :src="selectedScenario.image" alt="" :style="{ viewTransitionName: `scenario-${selectedScenario.id}` }" />
            <h3 class="panel-title"><span class="icon" :style="{ '--icon': `url(${selectedScenario.icon})` }" />{{ selectedScenario.name }}</h3>
          </div>
          <div class="card-body">
            <h3 class="card-title">{{ $t('common.game') }}</h3>
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
          </div>
        </section>

        <p v-if="error" class="error">{{ tr(error) }}</p>

        <button class="primary-action" type="submit" :disabled="!canCreate">
          {{ creating ? $t('newGame.creating') : $t('common.create') }}
        </button>
      </template>
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
.back-button {
  background: none;
  border: 0;
  border-radius: 3px;
  color: var(--text-muted);
  font: inherit;
  font-weight: bolder;
  padding: 10px 15px;
  text-transform: uppercase;
  cursor: pointer;
  transition: color 0.15s;
}
.back-button:hover { color: var(--text); }

#new-game {
  display: grid;
  gap: 16px;
  color: var(--text);
}

.card {
  display: grid;
  gap: 12px;
  padding: 16px;
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 8px;
}
.card-title {
  margin: 0;
  font-family: inherit;
  color: var(--title);
  font-size: 0.75rem;
  font-weight: 700;
  letter-spacing: 0.1em;
  text-transform: uppercase;
}

/* what was chosen down the left, from top to bottom, and the next choice beside it */
.card.split { grid-template-columns: 216px 1fr; gap: 0; padding: 0; overflow: hidden; }
.split .panel {
  display: flex;
  flex-direction: column;
  gap: 12px;
  padding: 16px;
  border-right: 1px solid var(--line-strong);
}
.split .card-body { display: grid; gap: 12px; align-content: start; padding: 16px; }
@media (max-width: 600px) {
  .card.split { grid-template-columns: 1fr; }
  .split .panel { border-right: 0; border-bottom: 1px solid var(--line-strong); }
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

.panel img { width: 100%; border-radius: 4px; box-shadow: 0 2px 8px var(--shadow); }
.panel-title { display: flex; align-items: center; gap: 6px; margin: 0; color: var(--title); font-size: 1.2em; }

.scenarios {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
  gap: 12px;
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
.scenario-box:not(:disabled):hover { border-color: var(--accent); }
.scenario-box.unavailable img { filter: grayscale(100%); }
.scenario-box.unavailable { cursor: not-allowed; opacity: 0.6; }
/* a ribbon across the corner, as in Arkham */
.scenario-box.alpha::after,
.scenario-box.beta::after {
  position: absolute;
  top: 9px;
  left: -22px;
  width: 86px;
  rotate: -45deg;
  color: var(--cream);
  font-size: 0.7rem;
  font-weight: 700;
  letter-spacing: 0.08em;
  line-height: 24px;
  text-align: center;
  text-transform: uppercase;
  box-shadow: 0 4px 10px var(--shadow);
}
.scenario-box.beta::after { content: 'beta'; background: darkgoldenrod; }
.scenario-box.alpha::after { content: 'alpha'; background: darkred; }
.scenario-name { display: flex; align-items: center; justify-content: center; gap: 6px; padding: 8px; font-weight: 700; text-align: center; }



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
