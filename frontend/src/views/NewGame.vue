<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useRouter } from 'vue-router'
import { createGame } from '../api'
import { currentUser } from '../auth'
import { campaigns, type Campaign, type ScenarioOption } from '../campaigns'

const router = useRouter()
const selectedCampaign = ref<Campaign | null>(null)
const selectedScenario = ref<ScenarioOption | null>(null)
const gameName = ref('')
const creating = ref(false)
const error = ref<string | null>(null)

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
    const { id } = await createGame({ name: gameName.value.trim() || defaultName.value })
    await router.push(`/games/${id}`)
  } catch (e) {
    error.value = e instanceof Error ? e.message : 'Could not create the game.'
    creating.value = false
  }
}
</script>

<template>
  <div class="new-game-content">
    <header class="main-header">
      <h2>New Game</h2>
      <button type="button" class="cancel-new-game-button" @click="cancel">Cancel</button>
    </header>

    <div v-if="!selectedCampaign" id="new-game">
      <fieldset>
        <legend>Campaign</legend>
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
            <span class="campaign-name">{{ c.name }}</span>
            <span class="campaign-count">
              {{ c.scenarios.length }} scenarios<template v-if="playableCount(c) > 0"> &middot; {{ playableCount(c) }} playable</template>
            </span>
          </button>
        </div>
      </fieldset>
    </div>

    <form v-else id="new-game" @submit.prevent="create">
      <fieldset>
        <legend>Scenario</legend>
        <div class="campaign-header">
          <img :src="selectedCampaign.image" alt="" />
          <div>
            <h3>{{ selectedCampaign.name }}</h3>
            <button type="button" class="change-campaign" @click="back">Choose a different campaign</button>
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
            <span class="scenario-name">{{ option.name }}</span>
            <span v-if="!option.available" class="coming-soon">Coming soon</span>
          </button>
        </div>
      </fieldset>

      <fieldset v-if="selectedScenario">
        <legend>Game</legend>
        <p class="deck">Playing as <strong>{{ currentUser?.username }}</strong> with <strong>Starter Set Leadership</strong> (Aragorn, Théodred, Glóin)</p>
        <label>
          Game name
          <input v-model="gameName" type="text" :placeholder="defaultName" />
        </label>
      </fieldset>

      <p v-if="error" class="error">{{ error }}</p>

      <div class="wizard-actions">
        <button type="button" class="secondary-action" @click="back">Back</button>
        <button class="primary-action" type="submit" :disabled="!canCreate">
          {{ creating ? 'Creating…' : 'Create' }}
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
.campaign-name { padding: 8px 8px 0; font-weight: 700; }
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
.scenario-name { padding: 8px; font-weight: 700; text-align: center; }
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

input[type='text'] {
  font: inherit;
  font-size: 1rem;
  color: var(--text);
  outline: 0;
  border: 1px solid var(--box-border);
  border-radius: 3px;
  padding: 12px;
  background: var(--input-background);
}
input[type='text']:focus { border-color: var(--accent); }
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
