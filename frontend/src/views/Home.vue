<script lang="ts" setup>
import { computed, ref } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRouter } from 'vue-router'
import { deleteGame, deleteCampaign, fetchEpicEvents, type EpicEvent, fetchGames, fetchCampaign, importGame, type CampaignView } from '../api'
import CampaignRow from '../components/CampaignRow.vue'
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
const epicEvents = ref<EpicEvent[]>([])
fetchEpicEvents().then(events => { epicEvents.value = events }).catch(() => {})
const error = ref<string | null>(null)

const campaigns = ref<Record<string, CampaignView>>({})
type HomeRow = { id: string; game: GameSummary; campaign?: CampaignView; games: GameSummary[] }
const rows = computed(() => {
  const result: HomeRow[] = []
  const seen = new Set<string>()
  for (const game of games.value) {
    const campaign = game.campaignId ? campaigns.value[game.campaignId] : undefined
    if (campaign) {
      if (seen.has(campaign.id)) continue
      seen.add(campaign.id)
      result.push({ id: `campaign-${campaign.id}`, campaign, game, games: games.value.filter(g => g.campaignId === campaign.id) })
    } else result.push({ id: game.id, game, games: [game] })
  }
  return result
})
const isFinished = (row: HomeRow) => row.campaign ? row.campaign.status === 'finished' : row.game.outcome !== null
const activeGames = computed(() => rows.value.filter(row => !isFinished(row)))
const finishedGames = computed(() => rows.value.filter(isFinished))

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
  .then(async (result) => {
    const ids = [...new Set(result.flatMap(game => game.campaignId ? [game.campaignId] : []))]
    const views = await Promise.all(ids.map(id => fetchCampaign(id)))
    campaigns.value = Object.fromEntries(views.map(view => [view.id, view]))
    games.value = result
  })
  .catch(() => (error.value = t('home.serverUnreachable')))

const removeGame = async (game: GameSummary) => {
  try {
    await deleteGame(game.id)
    games.value = games.value.filter((g) => g.id !== game.id)
    if (game.campaignId && games.value.some(g => g.campaignId === game.campaignId)) {
      campaigns.value[game.campaignId] = await fetchCampaign(game.campaignId)
    }
  } catch (e) {
    error.value = e instanceof Error ? e.message : t('home.serverUnreachable')
  }
}

const removeCampaign = async (campaign: CampaignView) => {
  try {
    await deleteCampaign(campaign.id)
    games.value = games.value.filter(game => game.campaignId !== campaign.id)
    delete campaigns.value[campaign.id]
  } catch (e) {
    error.value = e instanceof Error ? e.message : t('home.serverUnreachable')
  }
}

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

        <section v-if="epicEvents.length" class="epic-events" :aria-label="$t('epic.yourEvents')">
          <h3>{{ $t('epic.yourEvents') }}</h3>
          <RouterLink v-for="event in epicEvents" :key="event.id" :to="`/epic-events/${event.id}`"><strong>{{ event.name }}</strong><span>{{ $t('epic.groupSummary', { groups: event.groups.length, players: event.totalPlayers ?? event.groups.reduce((n, g) => n + g.seats, 0) }) }} →</span></RouterLink>
        </section>

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
          <template v-for="row in activeGames" :key="row.id">
            <CampaignRow v-if="row.campaign" :campaign="row.campaign" :games="row.games" @delete="removeCampaign(row.campaign)" />
            <GameRow v-else :game="row.game" :delete-game="() => removeGame(row.game)" />
          </template>
        </template>
        <template v-else>
          <div v-if="finishedGames.length === 0" class="box">
            <p>{{ $t('home.noFinishedGames') }}</p>
          </div>
          <template v-for="row in finishedGames" :key="row.id">
            <CampaignRow v-if="row.campaign" :campaign="row.campaign" :games="row.games" @delete="removeCampaign(row.campaign)" />
            <GameRow v-else :game="row.game" :delete-game="() => removeGame(row.game)" />
          </template>
        </template>
      </section>
    </div>
  </div>
</template>

<style scoped>
.epic-events { display: grid; gap: .5rem; margin-bottom: 1.25rem; }
.epic-events h3 { margin: 0; color: var(--title); font-size: 1rem; }
.epic-events a { display: flex; flex-wrap: wrap; justify-content: space-between; gap: .5rem; padding: .75rem 1rem; border: 1px solid var(--box-border); border-radius: 4px; background: var(--box-background); color: var(--text); text-decoration: none; }
.epic-events a:hover, .epic-events a:focus-visible { border-color: var(--accent); }
.epic-events span { color: var(--text-muted); font-size: .85rem; }
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
