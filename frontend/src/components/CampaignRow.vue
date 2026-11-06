<script setup lang="ts">
import { computed, useTemplateRef } from 'vue'
import ConfirmDelete from './ConfirmDelete.vue'
import TrashIcon from './TrashIcon.vue'
import { RouterLink } from 'vue-router'
import type { CampaignView } from '../api'
import type { GameSummary } from '../server'
import { slug } from '../campaigns'

const props = defineProps<{ campaign: CampaignView; games: GameSummary[] }>()
const emit = defineEmits<{ delete: [] }>()
const confirm = useTemplateRef<InstanceType<typeof ConfirmDelete>>('confirm')
const progress = computed(() => props.campaign.scenarios.filter(s => props.campaign.log.completed.some(c => c.scenario === s.id)).length)
const next = computed(() => props.campaign.scenarios.find(s => s.id === props.campaign.next)?.name)
const latest = computed(() => props.games.find(g => g.id === props.campaign.games[0]?.id) ?? props.games[0])
const activeGame = computed(() => props.campaign.status === 'playing' && latest.value?.outcome === null ? latest.value : null)
const activeIcon = computed(() => `url('/images/icons/scenarios/${slug(activeGame.value?.scenario ?? '')}.svg')`)
const phase = computed(() => `game.phases.${activeGame.value?.phase.replace(/Phase$/, '').toLowerCase()}`)
const icon = computed(() => `url('/images/icons/scenarios/${slug(props.campaign.scenarios[0]?.name ?? '')}.svg')`)
</script>

<template>
  <article class="campaign-row" :class="{ finished: campaign.status === 'finished' }">
    <header class="header-bar">
    <RouterLink class="campaign-header" :to="`/campaigns/${campaign.id}`">
    <div class="campaign-title">
      <span class="scenario-icon" role="img" :aria-label="campaign.campaignName" :style="{ '--icon': icon }" />
      <span class="title">{{ campaign.campaignName }}</span>
      <span class="badge progress">{{ $t('campaignMode.scenarios') }} {{ progress }} / {{ campaign.scenarios.length }}</span>
    </div>
    <div class="campaign-progress">
      <span v-if="campaign.status === 'finished'">{{ $t('campaignMode.finished') }}</span>
      <span v-else-if="campaign.status === 'won'">{{ $t('campaignMode.continue', { scenario: next }) }}</span>
      <span v-else-if="campaign.status === 'lost'">{{ $t('campaignMode.replay', { scenario: next }) }}</span>
    </div>
    </RouterLink>
    <button class="delete-campaign" type="button" :aria-label="$t('campaignMode.delete')" @click="confirm?.open()"><TrashIcon /></button>
    </header>
    <ConfirmDelete ref="confirm" :title="$t('campaignMode.confirmDelete')" @confirm="emit('delete')">
      <strong>{{ campaign.campaignName }}</strong>
    </ConfirmDelete>
    <component :is="activeGame ? RouterLink : 'div'" class="campaign-details" :class="{ 'active-scenario': activeGame }" :to="activeGame ? `/games/${activeGame.id}` : undefined">
      <template v-if="activeGame">
        <div class="scenario-heading">
          <span class="scenario-icon" aria-hidden="true" :style="{ '--icon': activeIcon }" />
          <span class="scenario-name">{{ activeGame.scenario }}</span>
          <span aria-hidden="true" class="open-game">↗</span>
        </div>
        <div class="scenario-progress">
          <span v-if="activeGame.waiting">{{ $t('home.waitingForPlayers') }}</span>
          <template v-else>{{ $t('game.round', { round: activeGame.round }) }} &middot; {{ $t(phase) }}</template>
        </div>
      </template>
    <ul v-if="latest?.seats?.length" class="seats">
      <li v-for="(seat, index) in latest.seats" :key="index" class="seat" :class="{ eliminated: seat.eliminated }">
        <span class="heroes">
          <span v-for="(hero, n) in seat.heroes" :key="`${hero.code}-${n}`" class="hero" :class="hero.sphere?.toLowerCase()" :style="{ backgroundImage: `url('/images/cards/${hero.code}.avif')` }" />
        </span>
        <span class="seat-caption">
          <span class="seat-name">{{ seat.name }}</span>
          <span class="seat-threat" :title="$t('game.threat', { value: seat.threat })">{{ seat.threat }}<span class="icon" :style="{ '--icon': `url('/images/icons/game/threat.svg')` }" /></span>
        </span>
      </li>
    </ul>
    </component>
  </article>
</template>

<style scoped>
.campaign-row { background: var(--box-background); border: 1px solid var(--box-border); border-left: 4px solid var(--accent); border-radius: 5px; padding: 0; margin-bottom: 10px; color: var(--text); }
.campaign-row.finished { border-left-color: var(--success); }
.campaign-title { display: flex; align-items: center; gap: 10px; flex-wrap: wrap; }
.scenario-icon { flex: none; width: 36px; height: 36px; background: var(--accent); mask: var(--icon) center / contain no-repeat; }
.title { color: var(--title); font-size: 1.4em; font-weight: 700; text-decoration: none; overflow-wrap: anywhere; }
.campaign-header:hover .title { color: var(--accent-bright); text-decoration: underline; }
.badge { flex: none; border: 1px solid var(--box-border); border-radius: 3px; color: var(--text-muted); font-size: .7em; font-weight: 700; padding: 1px 7px; text-transform: uppercase; letter-spacing: .06em; }
.progress { font-variant-numeric: tabular-nums; }
.campaign-progress { display: flex; gap: 16px; margin-top: 6px; color: var(--text-muted); font-size: .8em; text-transform: uppercase; letter-spacing: .05em; }
.campaign-header, .campaign-details { display: block; color: inherit; text-decoration: none; }
.header-bar { display: flex; align-items: center; background: color-mix(in oklab, var(--accent) 14%, var(--box-background)); border-radius: 0 4px 0 0; }
.campaign-header { flex: 1; min-width: 0; padding: 10px 14px; transition: background .2s; }
.campaign-header:hover { background: color-mix(in oklab, var(--accent) 8%, transparent); }
.delete-campaign { display: flex; flex: none; align-items: center; justify-content: center; width: 36px; height: 36px; margin-right: 8px; padding: 8px; background: transparent; border: 0; border-radius: 3px; color: var(--text-muted); cursor: pointer; }
.delete-campaign:hover { color: var(--danger); background: var(--subtle-tint); }
.delete-campaign svg { width: 18px; }
.delete-campaign:focus-visible { outline: 2px solid var(--accent); outline-offset: 2px; }
.campaign-details { border-top: 1px solid var(--box-border); padding: 10px 14px; }
.campaign-details:has(> .seats:only-child) { padding-top: 0; }
.campaign-details:empty { display: none; }
.active-scenario { transition: background .2s; }
.active-scenario:hover { background: var(--subtle-tint); }
.scenario-heading { display: flex; align-items: center; gap: 10px; }
.scenario-heading .scenario-icon { width: 28px; height: 28px; }
.scenario-name { color: var(--title); font-size: 1.1em; font-weight: 600; }
.open-game { margin-left: auto; color: var(--text-muted); }
.scenario-progress { color: var(--text-muted); font-size: .8em; margin-top: 6px; text-transform: uppercase; letter-spacing: .05em; }
.campaign-progress:empty { display: none; }
.seats { display: flex; flex-wrap: wrap; gap: 8px 18px; margin: 10px 0 0; padding: 8px 10px; list-style: none; background: rgb(229 211 173 / 0.03); border-radius: 4px; }
.seat { display: flex; flex-direction: column; gap: 4px; }
.seat.eliminated { opacity: .45; }
.heroes { display: grid; grid-auto-flow: column; grid-auto-columns: 40px; height: 60px; border-radius: 4px; overflow: hidden; background: var(--surface-2); box-shadow: 1px 1px 5px var(--shadow); }
.hero { --sphere-color: var(--neutral); background-size: 210% auto; background-position: 62% 0; border-bottom: 3px solid var(--sphere-color); }
.hero + .hero { box-shadow: -1px 0 0 rgb(0 0 0 / .5); }
.hero.leadership { --sphere-color: var(--leadership); }
.hero.lore { --sphere-color: var(--lore); }
.hero.spirit { --sphere-color: var(--spirit); }
.hero.tactics { --sphere-color: var(--tactics); }
.seat-caption { display: flex; align-items: center; justify-content: space-between; gap: 6px; }
.seat-name { color: var(--text); font-size: .85em; font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.seat-threat { display: flex; align-items: center; gap: 2px; color: var(--threat-red); font-size: .85em; font-weight: 700; }
.seat-threat .icon { display: inline-block; width: 1em; height: 1em; background: currentColor; mask: var(--icon) center / contain no-repeat; }
a:focus-visible { outline: 2px solid var(--accent); outline-offset: 4px; }
</style>
