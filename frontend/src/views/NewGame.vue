<script lang="ts" setup>
import { computed, nextTick, onMounted, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import { useRoute, useRouter } from 'vue-router'
import { createCampaign, createGame, createEpicEvent, fetchCampaign, type CampaignView } from '../api'
import { currentUser, updateSettings } from '../auth'
import { campaigns as allCampaigns, campaignModes, type ScenarioOption } from '../campaigns'
import { alphaEnabled, filterDisplayable, isDevBuild } from '../displayRules'
import { useServerText } from '../i18n'
import { useShortcuts } from '../browser/lib/keys'
import ScenarioRoutePreview from '../components/ScenarioRoutePreview.vue'

const tr = useServerText()


const { t } = useI18n()
const router = useRouter()
const route = useRoute()
const displayRules = {
  alpha: alphaEnabled(route.query),
  dev: isDevBuild(),
}
// the campaigns with something to show, and only what they have to show
const campaigns = allCampaigns
  .map((c) => ({ ...c, scenarios: filterDisplayable(c.scenarios, displayRules) }))
  .filter((c) => c.scenarios.length > 0)
const campaignGroups = [
  { id: 'campaign', title: 'newGame.campaigns', items: campaigns.filter((c) => c.category === 'campaign') },
  { id: 'scenario-pack', title: 'newGame.scenarioPacks', items: campaigns.filter((c) => c.category === 'scenario-pack') },
].filter((group) => group.items.length > 0)
const selectedCampaign = ref<(typeof campaigns)[number] | null>(null)
type Shown = ScenarioOption & { playable: boolean }
const selectedScenario = ref<Shown | null>(null)
const epic = ref(false)
const isAnnuminas = computed(() => selectedScenario.value?.id === 'the-siege-of-annuminas')
const isDolGuldur = computed(() => selectedScenario.value?.id === 'attack-on-dol-guldur')
const normalOnly = computed(() => isAnnuminas.value || isDolGuldur.value)
const offersEpic = computed(() => normalOnly.value || selectedScenario.value?.id === 'the-hunt-for-the-dreadnaught')
watch(selectedScenario, () => { epic.value = false })
const gameName = ref('')
const creating = ref(false)
// As in Arkham: decided when the game is made, starting from the player's default,
// which they can make whatever they picked here.
const allowSpectators = ref(currentUser.value?.allowSpectatorsByDefault === true)
const differsFromDefault = computed(() => allowSpectators.value !== (currentUser.value?.allowSpectatorsByDefault === true))
const savedDefault = ref(false)
// counted toward everyone's statistics until a debug message or an undo past something revealed
const publicScoring = ref(true)
watch(allowSpectators, () => (savedDefault.value = false))
const makeDefault = async () => {
  await updateSettings({ allowSpectatorsByDefault: allowSpectators.value })
  savedDefault.value = true
}
// normal, easy or nightmare: one at a time, nightmare only where the scenario has its deck
type Mode = 'normal' | 'easy' | 'nightmare' | 'hard'
const mode = ref<Mode>('normal')
const modes = computed(() =>
  (['easy', 'normal', 'hard', 'nightmare'] as const)
    .filter((m) => (!normalOnly.value || m === 'normal') && (m !== 'nightmare' || selectedScenario.value?.nightmare) && (m !== 'hard' || selectedScenario.value?.hard))
    .map((value) => ({ value, label: t(`newGame.mode.${value}`) })),
)
watch(selectedScenario, (s) => {
  if (normalOnly.value || (mode.value === 'nightmare' && !s?.nightmare) || (mode.value === 'hard' && !s?.hard)) mode.value = 'normal'
})
const error = ref<string | null>(null)
const rewardSource = ref<CampaignView | null>(null)
const includeReward = ref(true)
const chosenRewardSeats = ref([0, 1, 2, 3])
onMounted(async () => {
  const source = route.query.rewardCampaign
  if (typeof source !== 'string') return
  try {
    const campaign = await fetchCampaign(source)
    if (!campaign.canClaimRangerSense) throw new Error('$error.rewardUnavailable')
    rewardSource.value = campaign
  } catch (e) {
    includeReward.value = false
    error.value = e instanceof Error ? e.message : String(e)
  }
})

// Seats are taken, each with a deck, once the game is made, where its link brings
// friends to the open ones.
const seatCount = ref(1)

const playableCount = (c: (typeof campaigns)[number]) => c.scenarios.filter((s) => s.playable).length
const defaultName = computed(() => selectedScenario.value?.name ?? '')
const canCreate = computed(() => !!selectedScenario.value && selectedScenario.value.playable && !creating.value)

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

const chooseCampaign = (c: (typeof campaigns)[number]) =>
  step(() => {
    selectedCampaign.value = c
    selectedScenario.value = c.category === 'scenario-pack' && c.scenarios.length === 1
      ? c.scenarios[0] ?? null
      : null
  })

const chooseScenario = (s: Shown) => step(() => (selectedScenario.value = s))

// Campaign mode: the box's scenarios in order, from its first, as one campaign.
const asCampaign = ref(false)
const extendedMirkwoodPaths = ref(false)
const includeOldForest = ref(false)
const includeFog = ref(false)
const showCampaignChoices = ref(false)
watch(selectedCampaign, () => {
  extendedMirkwoodPaths.value = false
  includeOldForest.value = false
  includeFog.value = false
  showCampaignChoices.value = false
})
const offersMirkwoodPaths = computed(() => ['core-set', 'dark-of-mirkwood'].includes(selectedCampaign.value?.id ?? ''))
const offersOldForest = computed(() => selectedCampaign.value?.id === 'the-lord-of-the-rings')
const campaignMode = computed(() => extendedMirkwoodPaths.value && offersMirkwoodPaths.value
  ? 'mirkwood-paths'
  : offersOldForest.value && includeOldForest.value && includeFog.value ? 'the-lord-of-the-rings-with-the-old-forest-and-fog-on-the-barrow-downs'
  : offersOldForest.value && includeFog.value ? 'the-lord-of-the-rings-with-fog-on-the-barrow-downs'
  : offersOldForest.value && includeOldForest.value ? 'the-lord-of-the-rings-with-the-old-forest'
  : selectedCampaign.value ? campaignModes[selectedCampaign.value.id] ?? null : null)
const campaignTitle = computed(() => offersMirkwoodPaths.value
  ? t(extendedMirkwoodPaths.value ? 'campaignMode.mirkwoodPaths' : selectedCampaign.value?.id === 'core-set' ? 'campaignMode.coreCampaign' : 'campaignMode.miniCampaign')
  : selectedCampaign.value?.name ?? '')
const campaignRoute = computed(() => {
  if (offersOldForest.value && (includeOldForest.value || includeFog.value)) {
    const saga = selectedCampaign.value?.scenarios ?? []
    const forest = includeOldForest.value ? allCampaigns.find((c) => c.id === 'the-old-forest')?.scenarios ?? [] : []
    const fog = includeFog.value ? allCampaigns.find((c) => c.id === 'fog-on-the-barrow-downs')?.scenarios ?? [] : []
    return [...saga.slice(0, 1), ...forest, ...fog, ...saga.slice(1)]
  }
  if (!offersMirkwoodPaths.value) return allCampaigns.find((c) => c.id === selectedCampaign.value?.id)?.scenarios ?? []
  const core = allCampaigns.find((c) => c.id === 'core-set')?.scenarios.slice(0, 3) ?? []
  const dark = allCampaigns.find((c) => c.id === 'dark-of-mirkwood')?.scenarios ?? []
  return extendedMirkwoodPaths.value ? [...core, ...dark] : selectedCampaign.value?.id === 'core-set' ? core : dark
})
const chooseCampaignMode = () =>
  step(() => {
    asCampaign.value = true
    const first = extendedMirkwoodPaths.value
      ? campaigns.find((c) => c.id === 'core-set')?.scenarios[0]
      : selectedCampaign.value?.scenarios[0]
    selectedScenario.value = first ?? null
  })

// a step back: from the game's options to the scenarios, from there to the campaigns
const back = () =>
  step(() => {
    if (selectedCampaign.value?.category === 'scenario-pack' && selectedCampaign.value.scenarios.length === 1) {
      selectedScenario.value = null
      selectedCampaign.value = null
    } else if (selectedScenario.value) selectedScenario.value = null
    else selectedCampaign.value = null
    asCampaign.value = false
    error.value = null
  })
useShortcuts([{ key: 'Escape', description: 'Go back a step.', run: () => selectedCampaign.value && back() }])

const cancel = () => router.push('/')

const create = async () => {
  if (!canCreate.value) return
  creating.value = true
  error.value = null
  try {
    if (offersEpic.value && epic.value) {
      const event = await createEpicEvent({
        name: gameName.value.trim() || defaultName.value,
        scenario: selectedScenario.value?.id,
        difficulty: mode.value === 'easy' ? 'EasyMode' : mode.value === 'hard' ? 'HardMode' : 'NormalMode',
      })
      await router.push(`/epic-events/${event.id}`)
      return
    }
    const { id } = asCampaign.value && campaignMode.value
      ? await createCampaign({
          name: gameName.value.trim() || campaignTitle.value || defaultName.value,
          campaign: campaignMode.value,
          rewardCampaign: includeReward.value && chosenRewardSeats.value.some((seat) => seat < seatCount.value) ? rewardSource.value?.id : undefined,
          rewardSeats: chosenRewardSeats.value.filter((seat) => seat < seatCount.value),
          seats: seatCount.value,
          allowSpectators: allowSpectators.value,
          easyMode: mode.value === 'easy',
          nightmare: mode.value === 'nightmare',
          publicScoring: publicScoring.value,
        })
      : await createGame({
      name: gameName.value.trim() || defaultName.value,
      scenario: selectedScenario.value?.id,
      decks: [],
      seats: seatCount.value,
      allowSpectators: allowSpectators.value,
      easyMode: mode.value === 'easy',
      hardMode: mode.value === 'hard',
      nightmare: mode.value === 'nightmare',
      publicScoring: publicScoring.value,
    })
    await router.push(`/games/${id}`)
  } catch (e) {
    error.value = e instanceof Error ? e.message : t('newGame.couldNotCreateGame')
    creating.value = false
  }
}
</script>

<template>
  <div class="page-container">
    <header class="main-header">
      <h2 class="page-title">{{ $t('newGame.title') }}</h2>
      <button v-if="selectedCampaign" type="button" class="back-button" @click="back">{{ $t('common.back') }}</button>
      <button type="button" class="cancel-new-game-button" @click="cancel">{{ $t('common.cancel') }}</button>
    </header>

    <section v-if="rewardSource" class="card reward-banner">
      <label class="reward-choice"><input v-model="includeReward" type="checkbox" />{{ $t('campaignMode.includeRangerReward', { campaign: rewardSource.name }) }}</label>
      <p class="hint">{{ $t('campaignMode.rangerRewardNewHint') }}</p>
      <div v-if="includeReward" class="reward-seats">
        <label v-for="seat in seatCount" :key="seat" class="reward-choice"><input v-model="chosenRewardSeats" type="checkbox" :value="seat - 1" />{{ $t('campaignMode.player', { n: seat }) }}</label>
      </div>
    </section>
    <p v-if="error && !selectedScenario" class="error">{{ tr(error) }}</p>
    <div v-if="!selectedCampaign" id="new-game">
      <section v-for="group in campaignGroups" :key="group.id" class="card" :aria-labelledby="`new-game-${group.id}`">
        <h3 :id="`new-game-${group.id}`" class="card-title">{{ $t(group.title) }}</h3>
        <div class="campaigns">
          <button
            v-for="c in group.items"
            :key="c.id"
            type="button"
            class="campaign-box"
            :class="{ unavailable: playableCount(c) === 0, 'scenario-pack': c.category === 'scenario-pack' }"
            @click="chooseCampaign(c)"
          >
            <img :src="c.image" alt="" :style="{ viewTransitionName: `campaign-${c.id}` }" />
            <span class="campaign-name"><span class="icon" :style="{ '--icon': `url(${c.symbol})` }" />{{ c.name }}</span>
            <span v-if="c.scenarios.length > 1" class="campaign-count">
              {{ $t('newGame.scenarioCount', c.scenarios.length) }}<template v-if="playableCount(c) < c.scenarios.length"> &middot; {{ $t('newGame.playableCount', { count: playableCount(c) }) }}</template>
            </span>
          </button>
        </div>
      </section>
    </div>

    <form v-else id="new-game" @submit.prevent="create">
      <section v-if="!selectedScenario" class="card split" :class="{ 'expansion-selection': campaignMode }">
        <div class="panel">
          <h3 class="card-title">{{ $t(campaignMode ? 'campaignMode.expansionLabel' : 'newGame.campaign') }}</h3>
          <img :src="selectedCampaign.image" alt="" :style="{ viewTransitionName: `campaign-${selectedCampaign.id}` }" />
          <h3 class="panel-title"><span class="icon" :style="{ '--icon': `url(${selectedCampaign.symbol})` }" />{{ selectedCampaign.name }}</h3>
        </div>
        <div class="card-body" :class="{ 'play-choices': campaignMode }">
          <div v-if="campaignMode" class="play-mode">
            <div class="segmented" role="radiogroup" :aria-label="$t('campaignMode.playMode')">
              <button type="button" role="radio" :aria-checked="!showCampaignChoices" :class="{ selected: !showCampaignChoices }" @click="showCampaignChoices = false">{{ $t('campaignMode.singleScenario') }}</button>
              <button type="button" role="radio" :aria-checked="showCampaignChoices" :class="{ selected: showCampaignChoices }" @click="showCampaignChoices = true">{{ $t('campaignMode.campaignOption') }}</button>
            </div>
          </div>
          <section v-if="campaignMode && showCampaignChoices" class="campaign-path" aria-labelledby="campaign-path-title">
            <div class="choice-heading">
              <h3 id="campaign-path-title">{{ $t('campaignMode.playCampaign') }}<span v-if="!offersMirkwoodPaths" class="route-count">{{ $t('newGame.scenarioCount', campaignRoute.length) }}</span></h3>
              <p>{{ $t('campaignMode.campaignChoiceHint') }}</p>
            </div>
            <fieldset v-if="offersMirkwoodPaths" class="campaign-length">
              <legend class="visually-hidden">{{ $t('campaignMode.chooseRoute') }}</legend>
              <label class="route-option" :class="{ 'is-selected': !extendedMirkwoodPaths }">
                <input v-model="extendedMirkwoodPaths" type="radio" :value="false" />
                <span class="route-copy">
                  <strong>{{ $t(selectedCampaign.id === 'core-set' ? 'campaignMode.coreRouteTitle' : 'campaignMode.miniRouteTitle') }}</strong>
                  <span>{{ $t(selectedCampaign.id === 'core-set' ? 'campaignMode.coreRouteDescription' : 'campaignMode.miniRouteDescription') }}</span>
                </span>
                <span class="route-count">{{ $t('newGame.scenarioCount', selectedCampaign.id === 'core-set' ? 3 : 2) }}</span>
              </label>
              <label class="route-option" :class="{ 'is-selected': extendedMirkwoodPaths }">
                <input v-model="extendedMirkwoodPaths" type="radio" :value="true" />
                <span class="route-copy">
                  <strong>{{ $t('campaignMode.fullRouteTitle') }}</strong>
                  <span>{{ $t('campaignMode.fullRouteDescription') }}</span>
                </span>
                <span class="route-count">{{ $t('newGame.scenarioCount', 5) }}</span>
              </label>
            </fieldset>
            <label v-if="offersOldForest" class="reward-choice"><input v-model="includeOldForest" type="checkbox" />{{ $t('campaignMode.includeOldForest') }}</label>
            <label v-if="offersOldForest" class="reward-choice"><input v-model="includeFog" type="checkbox" />{{ $t('campaignMode.includeFog') }}</label>
            <div class="route-preview">
              <ScenarioRoutePreview :scenarios="campaignRoute" />
            </div>
            <button type="button" class="campaign-mode campaign-continue" @click="chooseCampaignMode">{{ $t('campaignMode.setUpCampaign') }}<span aria-hidden="true">→</span></button>
          </section>
          <section v-if="!campaignMode || !showCampaignChoices" :class="{ 'standalone-path': campaignMode }">
            <h3 class="card-title">{{ $t(campaignMode ? 'campaignMode.standalone' : 'common.scenario') }}</h3>
            <p v-if="campaignMode" class="standalone-hint">{{ $t('campaignMode.standaloneHint') }}</p>
            <div class="scenarios">
              <button
                v-for="option in selectedCampaign.scenarios"
                :key="option.id"
                type="button"
                class="scenario-box"
                :class="{ unavailable: !option.playable, alpha: option.alpha, beta: option.beta }"
                :disabled="!option.playable"
                @click="chooseScenario(option)"
              >
                <span class="scenario-art" :class="{ alpha: option.alpha, beta: option.beta }" :style="{ viewTransitionName: `scenario-${option.id}` }">
                  <img :src="option.image" alt="" />
                </span>
                <span class="scenario-name"><span class="icon" :style="{ '--icon': `url(${option.icon})` }" />{{ option.name }}</span>
              </button>
            </div>
          </section>
        </div>
      </section>

      <template v-else>
        <section class="card split" :class="{ 'expansion-selection': campaignMode }">
          <div class="panel">
            <h3 class="card-title">{{ $t('common.scenario') }}</h3>
            <span class="scenario-art" :class="{ alpha: selectedScenario.alpha, beta: selectedScenario.beta }" :style="{ viewTransitionName: `scenario-${selectedScenario.id}` }">
              <img :src="selectedScenario.image" alt="" />
            </span>
            <h3 class="panel-title"><span class="icon" :style="{ '--icon': `url(${selectedScenario.icon})` }" />{{ selectedScenario.name }}</h3>
          </div>
          <div class="card-body">
            <h3 class="card-title">{{ $t('common.game') }}</h3>
            <p v-if="selectedScenario.beta" class="beta-warning">{{ $t('newGame.betaWarning') }}</p>
            <div v-if="offersEpic" class="option">
              <span class="option-title">{{ $t('epic.playMode') }}</span>
              <div class="segmented" role="radiogroup" :aria-label="$t('epic.playMode')">
                <button type="button" role="radio" :aria-checked="!epic" :class="{ selected: !epic }" @click="epic = false">{{ $t('epic.singleGroup') }}</button>
                <button type="button" role="radio" :aria-checked="epic" :class="{ selected: epic }" @click="epic = true">{{ $t('epic.title') }}</button>
              </div>
              <p v-if="epic" class="hint">{{ $t(isAnnuminas ? 'epic.annuminasDescription' : isDolGuldur ? 'epic.dolGuldurDescription' : 'epic.description') }}</p>
            </div>
            <div v-if="asCampaign && selectedCampaign" class="campaign-summary">
              <span class="card-title">{{ $t('campaignMode.playCampaign') }}</span>
              <strong>{{ campaignTitle }}</strong>
              <details v-if="campaignRoute.length">
                <summary>{{ $t('campaignMode.viewRoute') }}</summary>
                <ScenarioRoutePreview class="summary-scenarios" :scenarios="campaignRoute" />
              </details>
              <p v-else class="hint">{{ $t('campaignMode.startHint', { campaign: campaignTitle }) }}</p>
            </div>
            <label>
              {{ $t('newGame.gameName') }}
              <input v-model="gameName" type="text" :placeholder="defaultName" />
            </label>
            <div v-if="!epic" class="option">
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
            <div class="option">
              <span class="option-title">{{ $t('newGame.mode.title') }}</span>
              <div class="segmented" role="radiogroup">
                <button
                  v-for="m in modes"
                  :key="m.value"
                  type="button"
                  role="radio"
                  :aria-checked="mode === m.value"
                  :class="{ selected: mode === m.value }"
                  @click="mode = m.value"
                >{{ m.label }}</button>
              </div>
              <p class="hint">{{ $t(asCampaign && mode === 'nightmare' ? 'newGame.mode.nightmareCampaignHint' : `newGame.mode.${mode}Hint`) }}</p>
            </div>
            <div v-if="!epic" class="toggle-row">
              <label class="switch">
                <input v-model="allowSpectators" type="checkbox" role="switch" />
                <span class="track" aria-hidden="true" />
                {{ $t('newGame.allowSpectators') }}
              </label>
              <button v-if="differsFromDefault" type="button" class="make-default" @click="makeDefault">{{ $t('newGame.makeDefault') }}</button>
              <span v-else-if="savedDefault" class="saved">{{ $t('newGame.defaultSaved') }}</span>
              <p class="hint">{{ $t('newGame.allowSpectatorsHint') }}</p>
            </div>
            <div v-if="!epic" class="toggle-row">
              <label class="switch">
                <input v-model="publicScoring" type="checkbox" role="switch" />
                <span class="track" aria-hidden="true" />
                {{ $t('newGame.publicScoring') }}
              </label>
              <p class="hint">{{ $t('newGame.publicScoringHint') }}</p>
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
  margin-bottom: 12px;
}

/* the header takes the room below it; the buttons sit at its far end */
.main-header .page-title { margin: 0 auto 0 0; }

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
.campaign-box.scenario-pack { align-self: start; }
.campaign-box.scenario-pack img { aspect-ratio: auto; height: auto; object-fit: contain; }
.campaign-box:hover { border-color: var(--accent); transform: translateY(-2px); }
.campaign-box.unavailable img { filter: grayscale(70%); }
.campaign-name { display: flex; align-items: center; gap: 6px; padding: 8px 8px 0; font-weight: 700; text-align: left; }
.campaign-name:last-child { padding-bottom: 8px; }
.campaign-name .icon, .scenario-name .icon { font-size: 1.2em; }
.campaign-count { padding: 2px 8px 8px; font-size: 0.75em; color: var(--text-muted); text-transform: uppercase; letter-spacing: 0.04em; }

.panel > img, .panel > .scenario-art { width: 100%; border-radius: 4px; box-shadow: 0 2px 8px var(--shadow); }
.panel-title { display: flex; align-items: center; gap: 6px; margin: 0; color: var(--title); font-size: 1.2em; }

.scenarios {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(0, 150px));
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
/* adventure packs' art is portrait and deluxe boxes' square: each sits whole in the
   middle of the same square, padded out to it */
.scenario-box .scenario-art { aspect-ratio: 1; }
.scenario-box img { width: 100%; aspect-ratio: 1; box-sizing: border-box; padding: 8px; object-fit: contain; display: block; }
.scenario-box:not(:disabled):hover { border-color: var(--accent); }
.scenario-box.unavailable img { filter: grayscale(100%); }
.scenario-box.unavailable { cursor: not-allowed; opacity: 0.6; }
/* a ribbon across the corner, as in Arkham */
/* Capture the art and its ribbon together: named transition snapshots are painted
   above the page, so a ribbon on the outer button would fall behind the image. */
.scenario-art { display: block; position: relative; isolation: isolate; overflow: hidden; width: 100%; border-radius: 4px; }
.scenario-art img { display: block; width: 100%; }
.scenario-art.alpha::after,
.scenario-art.beta::after {
  position: absolute;
  z-index: 1;
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
.scenario-art.beta::after { content: 'beta'; background: darkgoldenrod; }
.scenario-art.alpha::after { content: 'alpha'; background: darkred; }
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

.reward-banner { width: 70vw; max-width: 98vw; margin: 0 auto 16px; box-sizing: border-box; }
.reward-seats { display: flex; flex-wrap: wrap; gap: 12px; }
.reward-choice { display: flex; align-items: center; gap: 8px; color: var(--text); }
@media (max-width: 768px) { .reward-banner { width: 100%; } }
.campaign-mode { padding: 10px; border: 1px solid var(--accent); border-radius: 3px; background: none; color: var(--accent); font: inherit; font-weight: 700; cursor: pointer; }
.campaign-mode:hover { background: var(--accent); color: var(--button-primary-text); }

.split .play-choices { gap: 0; padding: 0; }
.card-body > section:not(.campaign-path):not(.standalone-path) { display: grid; gap: 12px; }
.play-mode { padding: 24px 28px 0; }
.play-mode .segmented button { padding: 12px 16px; }
.play-mode button:focus-visible { outline: 2px solid var(--text); outline-offset: -4px; }
.campaign-path { padding: 24px 28px 28px; }
.choice-heading h3 { display: flex; align-items: center; justify-content: space-between; gap: 12px; margin: 0; font-size: 1.4rem; color: var(--title); line-height: 1.25; }
.choice-heading p { margin: 8px 0 22px; color: var(--text-muted); font-size: 0.9rem; line-height: 1.55; max-width: 52ch; }
.campaign-length { display: grid; gap: 10px; margin: 0; padding: 0; border: 0; min-width: 0; }
.route-option { display: grid; grid-template-columns: auto 1fr auto; gap: 12px; align-items: center; padding: 16px; border: 1px solid var(--box-border); border-radius: 5px; cursor: pointer; background: var(--input-background); transition: border-color 160ms, background 160ms; }
.route-option:hover { border-color: var(--text-muted); }
.route-option.is-selected { border-color: var(--accent); background: var(--box-background); box-shadow: inset 3px 0 var(--accent); }
.route-option input { margin: 0; accent-color: var(--accent); width: 16px; height: 16px; }
.route-copy { display: grid; gap: 5px; }
.route-copy strong { color: var(--text); font-size: 1rem; font-weight: 600; }
.route-copy > span { color: var(--text-muted); font-size: 0.8rem; line-height: 1.45; }
.route-count { color: var(--text-muted); font-size: 0.75rem; white-space: nowrap; }
.is-selected .route-copy strong, .is-selected .route-count { color: var(--accent); }
.route-preview { margin: 20px 0; }
.campaign-continue { display: flex; align-items: center; justify-content: space-between; gap: 20px; width: 100%; padding: 13px 16px; background: var(--button-primary); border-color: transparent; color: var(--button-primary-text); }
.campaign-continue:hover { background: var(--button-primary-hover); }
.standalone-path { padding: 24px 28px 28px; }
.standalone-path > .card-title { color: var(--text); font-size: 1rem; text-transform: none; letter-spacing: normal; font-weight: 600; }
.standalone-hint { margin: 6px 0 14px; font-size: 0.8rem; color: var(--text-muted); line-height: 1.5; }
.campaign-summary { display: grid; gap: 8px; padding-bottom: 16px; margin-bottom: 4px; border-bottom: 1px solid var(--line-strong); }
.campaign-summary > strong { color: var(--title); font-size: 1.1rem; }
.campaign-summary summary { color: var(--text-muted); font-size: 0.8rem; cursor: pointer; }
.campaign-summary .summary-scenarios { margin-top: 12px; }
.route-option:focus-within, .campaign-continue:focus-visible, .scenario-box:focus-visible { outline: 2px solid var(--accent); outline-offset: 3px; }
.visually-hidden { position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px; overflow: hidden; clip-path: inset(50%); white-space: nowrap; border: 0; }
@media (max-width: 900px) {
  .route-option { grid-template-columns: auto 1fr; }
  .route-count { grid-column: 2; }
}
@media (max-width: 600px) {
  .expansion-selection .panel { display: grid; grid-template-columns: 64px 1fr; column-gap: 16px; align-items: center; }
  .expansion-selection .panel > img, .expansion-selection .panel > .scenario-art { grid-column: 1; grid-row: 1 / 3; width: 64px; }
  .expansion-selection .panel .card-title { grid-column: 2; align-self: end; }
  .expansion-selection .panel-title { grid-column: 2; align-self: start; font-size: 1rem; }
  .campaign-path, .standalone-path { padding: 20px; }
  .play-mode { padding: 20px 20px 0; }
}
.make-default { background: none; border: 0; padding: 0; color: var(--accent); font: inherit; text-decoration: underline; cursor: pointer; }
.saved { color: var(--text-muted); }
.hint { margin: 0; font-size: 0.8rem; }
.beta-warning { margin: 0; padding: 8px 12px; border-left: 4px solid darkgoldenrod; background: color-mix(in srgb, darkgoldenrod 15%, transparent); }



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
