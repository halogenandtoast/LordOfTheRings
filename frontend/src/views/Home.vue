<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRouter } from 'vue-router'
import { deleteGame, fetchGames, importGame } from '../api'
import GameRow from '../components/GameRow.vue'
import PrimaryButton from '../components/PrimaryButton.vue'
import SegmentedToggle from '../components/SegmentedToggle.vue'
import type { CardDef, GameSummary } from '../server'
import { importPreview } from '../importPreview'
import { useServerText } from '../i18n'

const tr = useServerText()


type Tab = 'active' | 'finished'

const { t } = useI18n()
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
  { value: 'active' as Tab, label: t('home.activeGames'), count: activeGames.value.length },
  { value: 'finished' as Tab, label: t('home.finishedGames'), count: finishedGames.value.length },
])

fetchGames()
  .then((result) => (games.value = result))
  .catch(() => (error.value = t('home.serverUnreachable')))

const removeGame = (game: GameSummary) =>
  deleteGame(game.id).then(() => {
    games.value = games.value.filter((g) => g.id !== game.id)
  })

const goToNewGame = () => router.push('/new-game')

const showImportGame = ref(false)
const importFile = ref<File | null>(null)
const importing = ref(false)
const importError = ref<string | null>(null)
const preview = ref<GameSummary | null>(null)
const readingFile = ref(false)
let selection = 0
let cardDefinitions: Promise<Record<string, CardDef>> | undefined

const chooseFile = async (event: Event) => {
  const currentSelection = ++selection
  const file = (event.target as HTMLInputElement).files?.[0] ?? null
  importFile.value = null
  preview.value = null
  importError.value = null
  readingFile.value = !!file
  if (!file) return
  try {
    cardDefinitions ??= fetch('/api/cards').then((response) => {
      if (!response.ok) throw new Error('Could not load cards')
      return response.json() as Promise<Record<string, CardDef>>
    }).catch(() => ({}))
    const [contents, cards] = await Promise.all([file.text(), cardDefinitions])
    const summary = importPreview(JSON.parse(contents), cards)
    if (currentSelection !== selection) return
    preview.value = summary
    importFile.value = file
  } catch {
    if (currentSelection === selection) importError.value = t('home.couldNotLoadFile')
  } finally {
    if (currentSelection === selection) readingFile.value = false
  }
}

const submitImport = async () => {
  if (!importFile.value) return
  importing.value = true
  importError.value = null
  try {
    const { id } = await importGame(importFile.value)
    await router.push(`/games/${id}`)
  } catch (e) {
    importError.value = e instanceof Error ? e.message : t('home.couldNotLoadFile')
  } finally {
    importing.value = false
  }
}
</script>

<template>
  <div class="page-container">
    <div class="home page-content">
      <section>
        <header class="main-header">
          <div class="title-group">
            <h2 class="page-title">{{ $t('home.yourGames') }}</h2>
            <SegmentedToggle
              class="tabs"
              :model-value="tab"
              :options="tabOptions"
              :label="$t('home.games')"
              @update:model-value="chosenTab = $event as Tab"
            />
          </div>
          <div class="header-actions">
            <button class="secondary-cta" type="button" @click="showImportGame = !showImportGame">{{ $t('home.loadGame') }}</button>
            <PrimaryButton :label="$t('newGame.title')" @click="goToNewGame" />
          </div>
        </header>

        <Transition name="slide">
          <div v-if="showImportGame" class="load-game-panel">
            <div class="panel-header">
              <h3>{{ $t('home.loadGame') }}</h3>
              <div class="panel-actions">
                <button class="panel-close" type="button" @click="showImportGame = false">{{ $t('common.cancel') }}</button>
                <button v-if="preview" class="panel-load" type="button" :disabled="importing || readingFile" @click="submitImport">
                  {{ importing ? $t('home.loading') : $t('home.loadGame') }}
                </button>
              </div>
            </div>
            <GameRow v-if="preview" class="import-preview" :game="preview" preview />
            <label class="file-picker" :class="{ compact: preview }">
              <input type="file" accept="application/json,.json" :disabled="importing" @change="chooseFile" />
              <span>{{ readingFile ? $t('home.loading') : $t('home.chooseFile') }}</span>
            </label>
            <p v-if="importError" class="import-error">{{ tr(importError) }}</p>
          </div>
        </Transition>

        <div v-if="error" class="box error">
          <p>{{ tr(error) }}</p>
        </div>

        <template v-if="tab === 'active'">
          <div v-if="activeGames.length === 0" class="box">
            <p>{{ $t('home.noActiveGames') }}</p>
          </div>
          <GameRow v-for="game in activeGames" :key="game.id" :game="game" :delete-game="() => removeGame(game)" />
        </template>
        <template v-else>
          <div v-if="finishedGames.length === 0" class="box">
            <p>{{ $t('home.noFinishedGames') }}</p>
          </div>
          <GameRow v-for="game in finishedGames" :key="game.id" :game="game" :delete-game="() => removeGame(game)" />
        </template>
      </section>
    </div>
  </div>
</template>

<style scoped>
.home {
  --segmented-accent: var(--button-secondary);
  --segmented-accent-highlight: var(--button-secondary-hover);
  --segmented-text: var(--text);
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
  margin-bottom: 12px;
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

/* the header takes the room below it */
.main-header .page-title { margin: 0; }

@media (max-width: 768px) {
  h2 { font-size: 1.5em; }
  header { flex-wrap: wrap; }
  .tabs { min-width: 0; }
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

.secondary-cta {
  background: transparent;
  border: 1px solid var(--box-border);
  border-radius: 3px;
  color: var(--title);
  cursor: pointer;
  font: inherit;
  font-size: 0.85em;
  font-weight: 700;
  padding: 8px 12px;
  text-transform: uppercase;
}
.secondary-cta:hover { background: var(--subtle-tint); }

.load-game-panel {
  background: var(--box-background);
  border: 1px solid var(--box-border);
  border-radius: 6px;
  padding: 12px;
  margin-bottom: 12px;
  box-shadow: 0 8px 24px var(--shadow);
}

.panel-header {
  display: flex;
  align-items: center;
  gap: 12px;
  margin-bottom: 10px;
}
.panel-header h3 {
  flex: 1;
  margin: 0;
  color: var(--title);
  font-size: 1.2em;
  text-transform: uppercase;
}
.panel-actions { display: flex; gap: 8px; }
.panel-load,
.panel-close {
  border: 1px solid var(--box-border);
  border-radius: 3px;
  cursor: pointer;
  font: inherit;
  font-size: 0.8em;
  font-weight: 700;
  padding: 7px 10px;
  text-transform: uppercase;
  color: var(--text);
  background: transparent;
}
.panel-load { background: var(--button-primary); border-color: var(--button-primary); color: var(--button-primary-text); }
.panel-load:hover:not(:disabled) { background: var(--button-primary-hover); }
.panel-load:disabled { opacity: 0.55; cursor: not-allowed; }
.panel-close:hover { background: var(--subtle-tint); }

.file-picker {
  display: block;
  border: 1px dashed var(--box-border);
  border-radius: 5px;
  padding: 18px;
  text-align: center;
  color: var(--text-muted);
  cursor: pointer;
}
.file-picker:hover { border-color: var(--accent); color: var(--text); }
.file-picker input { display: none; }
.file-picker.compact { padding: 8px; font-size: 0.8em; }
.import-preview { margin-bottom: 8px; }

.import-error { color: var(--danger); margin-top: 8px; }

.slide-enter-active,
.slide-leave-active { transition: opacity 0.3s ease-in-out; }
.slide-enter-from,
.slide-leave-to { opacity: 0; }
</style>
