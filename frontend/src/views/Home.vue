<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useRouter } from 'vue-router'
import { deleteGame, fetchGames } from '../api'
import GameRow from '../components/GameRow.vue'
import PrimaryButton from '../components/PrimaryButton.vue'
import SegmentedToggle from '../components/SegmentedToggle.vue'
import type { GameSummary } from '../server'

type Tab = 'active' | 'finished'

const router = useRouter()
const games = ref<GameSummary[]>([])
const error = ref<string | null>(null)

const activeGames = computed(() => games.value.filter((g) => g.outcome === null))
const finishedGames = computed(() => games.value.filter((g) => g.outcome !== null))

// Null until the player picks a tab, so they land on whichever tab has something in it.
const chosenTab = ref<Tab | null>(null)
const tab = computed<Tab>(() => {
  if (chosenTab.value) return chosenTab.value
  return activeGames.value.length === 0 && finishedGames.value.length > 0 ? 'finished' : 'active'
})

const tabOptions = computed(() => [
  { value: 'active' as Tab, label: 'Active games', count: activeGames.value.length },
  { value: 'finished' as Tab, label: 'Finished games', count: finishedGames.value.length },
])

fetchGames()
  .then((result) => (games.value = result))
  .catch(() => (error.value = 'The server could not be reached.'))

const removeGame = (game: GameSummary) =>
  deleteGame(game.id).then(() => {
    games.value = games.value.filter((g) => g.id !== game.id)
  })

const goToNewGame = () => router.push('/new-game')
</script>

<template>
  <div class="page-container">
    <div class="home page-content">
      <section>
        <header class="main-header">
          <div class="title-group">
            <h2>Your Games</h2>
            <SegmentedToggle
              class="tabs"
              :model-value="tab"
              :options="tabOptions"
              label="Games"
              @update:model-value="chosenTab = $event as Tab"
            />
          </div>
          <div class="header-actions">
            <PrimaryButton label="New Game" @click="goToNewGame" />
          </div>
        </header>

        <div v-if="error" class="box error">
          <p>{{ error }}</p>
        </div>

        <template v-if="tab === 'active'">
          <div v-if="activeGames.length === 0" class="box">
            <p>You have no games in progress. Start a new one to set out on a quest.</p>
          </div>
          <GameRow v-for="game in activeGames" :key="game.id" :game="game" :delete-game="() => removeGame(game)" />
        </template>
        <template v-else>
          <div v-if="finishedGames.length === 0" class="box">
            <p>No finished games yet.</p>
          </div>
          <GameRow v-for="game in finishedGames" :key="game.id" :game="game" :delete-game="() => removeGame(game)" />
        </template>
      </section>
    </div>
  </div>
</template>

<style scoped>
.page-container {
  flex: 1;
  min-height: 0;
  overflow-y: auto;
  scrollbar-gutter: stable;
  padding: 20px 10px 10px;
}

.home {
  --segmented-accent: var(--button-secondary);
  --segmented-accent-highlight: var(--button-secondary-hover);
  max-width: 98vw;
  width: 70vw;
  min-width: 60vw;
  margin: 0 auto;
}

@media (max-width: 768px) {
  .home { min-width: unset; width: 100%; }
}

header {
  display: flex;
  margin-bottom: 10px;
  align-items: center;
  gap: 12px;
}

.title-group {
  display: flex;
  align-items: center;
  gap: 16px;
  flex: 1;
  flex-wrap: wrap;
}

.tabs { min-width: 18rem; }

h2 {
  color: var(--title);
  font-size: 2em;
  text-transform: uppercase;
  letter-spacing: 0.04em;
  margin: 0;
}

@media (max-width: 768px) {
  h2 { font-size: 1.5em; }
}

.header-actions {
  display: flex;
  align-items: center;
  gap: 8px;
}

.box {
  background-color: var(--box-background);
  border: 1px solid var(--box-border);
  color: var(--text);
  padding: 14px;
  border-radius: 5px;
}
.box.error { border-color: var(--danger); margin-bottom: 10px; }

p { margin: 0; }
</style>
