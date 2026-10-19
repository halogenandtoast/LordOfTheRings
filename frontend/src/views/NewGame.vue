<script lang="ts" setup>
import { computed, onMounted, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRouter } from 'vue-router'
import { createGame, type DeckChoice } from '../api'
import { fetchDecks } from '../browser/lib/api'
import type { DeckSummary } from '../browser/types/deck'
import { currentUser } from '../auth'
import { campaigns, type Campaign, type ScenarioOption } from '../campaigns'
import { useServerText } from '../i18n'

const tr = useServerText()


const { t } = useI18n()
const router = useRouter()
const selectedCampaign = ref<Campaign | null>(null)
const selectedScenario = ref<ScenarioOption | null>(null)
const gameName = ref('')
const creating = ref(false)
const error = ref<string | null>(null)

const starterSpheres = ['leadership', 'tactics', 'spirit', 'lore'] as const
const decks = ref<DeckSummary[]>([])
// a select's value is a string: `starter:<sphere>` or `deck:<id>`
const deckChoice = ref('starter:leadership')
const toDeckChoice = (value: string): DeckChoice => {
  const [kind, rest] = value.split(':')
  return kind === 'deck' ? { deckId: Number(rest) } : { starter: rest }
}
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
      deck: toDeckChoice(deckChoice.value),
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

      <fieldset v-if="selectedScenario">
        <legend>{{ $t('common.game') }}</legend>
        <i18n-t keypath="newGame.playingAs" tag="p" class="deck">
          <template #player><strong>{{ currentUser?.username }}</strong></template>
        </i18n-t>
        <label>
          {{ $t('newGame.deck') }}
          <select v-model="deckChoice">
            <optgroup :label="$t('newGame.starterDecks')">
              <option v-for="sphere in starterSpheres" :key="sphere" :value="`starter:${sphere}`">
                {{ $t(`newGame.starter.${sphere}`) }}
              </option>
            </optgroup>
            <optgroup v-if="decks.length" :label="$t('newGame.yourDecks')">
              <option v-for="d in decks" :key="d.id" :value="`deck:${d.id}`">{{ d.name }} ({{ d.heroNames.join(', ') }})</option>
            </optgroup>
          </select>
        </label>
        <label>
          {{ $t('newGame.gameName') }}
          <input v-model="gameName" type="text" :placeholder="defaultName" />
        </label>
      </fieldset>

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
