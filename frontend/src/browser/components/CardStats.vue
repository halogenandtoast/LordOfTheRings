<script setup lang="ts">
/**
 * The stat line: whichever of the printed statistics a card actually has.
 *
 * Characters show willpower / attack / defence / hit points; enemies add
 * engagement cost and threat; locations show threat and quest points.
 */
import { computed } from 'vue'
import { useI18n } from 'vue-i18n'
import GameIcon from './GameIcon.vue'
import type { Card, Stat } from '@browser/types/card'
import type { IconName } from '@browser/lib/card-text'

const { card, compact = false } = defineProps<{ card: Card; compact?: boolean }>()

/** Short forms for the stats with no glyph of their own. */
const ABBREVIATIONS: Record<string, string> = {
  engagement: 'browser.stats.abbreviations.engagement',
  health: 'browser.stats.abbreviations.health',
  quest: 'browser.stats.abbreviations.quest',
  victory: 'browser.stats.abbreviations.victory',
}

const { t } = useI18n()

interface StatEntry {
  key: string
  icon: IconName | null
  label: string
  value: Stat
}

const stats = computed<StatEntry[]>(() => {
  const entries: StatEntry[] = []
  const add = (key: string, icon: IconName | null, label: string, value: Stat | null) => {
    if (value !== null) entries.push({ key, icon, label, value })
  }

  add('engagement', null, t('browser.stats.engagementCost'), card.engagementCost)
  add('threat', 'threat', t('browser.stats.threat'), card.threat)
  add('willpower', 'willpower', t('browser.stats.willpower'), card.willpower)
  add('attack', 'attack', t('browser.stats.attack'), card.attack)
  add('defense', 'defense', t('browser.stats.defense'), card.defense)
  add('health', null, t('browser.stats.hitPoints'), card.health)
  add('quest', null, t('browser.stats.questPoints'), card.questPoints)
  add('victory', null, t('browser.stats.victoryPoints'), card.victoryPoints)

  return entries
})
</script>

<template>
  <ul v-if="stats.length" class="stats" :class="{ 'stats--compact': compact }">
    <li v-for="stat in stats" :key="stat.key" class="stat" :data-stat="stat.key">
      <GameIcon v-if="stat.icon" :name="stat.icon" :label="stat.label" />
      <abbr v-else :title="stat.label">{{ $t(ABBREVIATIONS[stat.key]!) }}</abbr>
      <span class="stat__value">{{ stat.value }}</span>
    </li>
  </ul>
</template>

<style scoped>
.stats {
  display: flex;
  flex-wrap: wrap;
  gap: var(--space-2);
  font-variant-numeric: tabular-nums;
}

.stat {
  display: inline-flex;
  align-items: center;
  gap: 0.2em;
  font-size: var(--text-sm);
  color: var(--c-text-muted);
}

.stat__value {
  color: var(--c-text);
  font-weight: 600;
}

abbr {
  font-size: var(--text-xs);
  font-weight: 600;
  letter-spacing: 0.03em;
  text-decoration: none;
  cursor: help;
}

.stat[data-stat='engagement'] {
  color: var(--c-enemy);
}

.stats--compact {
  gap: var(--space-3);
}

.stats--compact .stat {
  font-size: var(--text-xs);
}
</style>
