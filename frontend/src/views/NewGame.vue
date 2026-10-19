<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useRouter } from 'vue-router'
import { createGame } from '../api'

interface ScenarioOption {
  id: string
  name: string
  image: string
  available: boolean
}

// Only Passage Through Mirkwood is scripted so far; the other core scenarios are shown
// so it is clear what is coming.
const scenarios: ScenarioOption[] = [
  { id: 'passage-through-mirkwood', name: 'Passage Through Mirkwood', image: '/images/cards/01119.jpg', available: true },
  { id: 'journey-along-the-anduin', name: 'Journey Along the Anduin', image: '/images/cards/01126.jpg', available: false },
  { id: 'escape-from-dol-guldur', name: 'Escape from Dol Guldur', image: '/images/cards/01123.jpg', available: false },
]

const router = useRouter()
const selectedScenario = ref(scenarios[0])
const playerName = ref('')
const gameName = ref('')
const seed = ref('')
const creating = ref(false)
const error = ref<string | null>(null)

const defaultName = computed(() => selectedScenario.value.name)
const canCreate = computed(() => selectedScenario.value.available && playerName.value.trim() !== '' && !creating.value)

const cancel = () => router.push('/')

const create = async () => {
  if (!canCreate.value) return
  creating.value = true
  error.value = null
  try {
    const { id } = await createGame({
      name: gameName.value.trim() || defaultName.value,
      playerName: playerName.value.trim(),
      seed: seed.value.trim() === '' ? undefined : Number(seed.value),
    })
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

    <form id="new-game" @submit.prevent="create">
      <fieldset>
        <legend>Scenario</legend>
        <div class="scenarios">
          <button
            v-for="option in scenarios"
            :key="option.id"
            type="button"
            class="scenario-box"
            :class="{ selected: option.id === selectedScenario.id, unavailable: !option.available }"
            :disabled="!option.available"
            :aria-pressed="option.id === selectedScenario.id"
            @click="selectedScenario = option"
          >
            <img :src="option.image" alt="" />
            <span class="scenario-name">{{ option.name }}</span>
            <span v-if="!option.available" class="coming-soon">Coming soon</span>
          </button>
        </div>
      </fieldset>

      <fieldset>
        <legend>Players</legend>
        <label>
          Your name
          <input v-model="playerName" type="text" placeholder="Frodo" required />
        </label>
        <p class="deck">Deck: <strong>Starter Set Leadership</strong> (Aragorn, Théodred, Glóin)</p>
      </fieldset>

      <fieldset>
        <legend>Game</legend>
        <label>
          Game name
          <input v-model="gameName" type="text" :placeholder="defaultName" />
        </label>
        <label>
          Seed <span class="optional">(optional, replays the same shuffle)</span>
          <input v-model="seed" type="text" inputmode="numeric" pattern="-?[0-9]*" placeholder="random" />
        </label>
      </fieldset>

      <p v-if="error" class="error">{{ error }}</p>

      <div class="wizard-actions">
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

.scenarios {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
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
.scenario-box img { width: 100%; aspect-ratio: 1 / 0.715; object-fit: cover; display: block; }
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

@media (max-width: 768px) {
  .scenarios { grid-template-columns: 1fr; }
}

label {
  display: grid;
  gap: 4px;
  font-size: 0.85rem;
  color: var(--text-muted);
}
.optional { opacity: 0.8; font-size: 0.9em; }

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
