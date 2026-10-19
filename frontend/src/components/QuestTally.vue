<script setup lang="ts">
import type { TallyView } from '../types'

defineProps<{ tally: TallyView }>()

const max = 50
const steps = Array.from({ length: max + 1 }, (_, n) => n)
// totals past the end of the track sit on its last space
const at = (value: number) => Math.min(value, max)
const willpowerIcon = { '--icon': "url('/images/icons/game/willpower.svg')" }
const threatIcon = { '--icon': "url('/images/icons/game/threat.svg')" }
</script>

<template>
  <section class="tally" aria-label="Quest tally">
    <p class="summary">
      <span class="total"><span class="icon marker willpower" :style="willpowerIcon" /> {{ tally.willpower }}</span>
      <span v-if="tally.lastWillpower !== null" class="total last-round">last round {{ tally.lastWillpower }}</span>
      <span class="total"><span class="icon marker threat" :style="threatIcon" /> {{ tally.threat }}</span>
    </p>
    <ol class="track">
      <li
        v-for="n in steps"
        :key="n"
        :class="{
          tick: n % 5 === 0,
          ahead: n <= at(tally.willpower) && n > at(tally.threat),
          'on-willpower': n === at(tally.willpower),
          'on-threat': n === at(tally.threat),
        }"
      >
        <span class="slot">
          <span v-if="n === at(tally.willpower)" class="icon marker willpower" :style="willpowerIcon" :title="`Willpower ${tally.willpower}`" />
          <span v-else-if="n === at(tally.lastWillpower ?? -1)" class="icon marker last" :style="willpowerIcon" :title="`Last round ${tally.lastWillpower}`" />
        </span>
        <span class="number">{{ n }}</span>
        <span class="slot">
          <span v-if="n === at(tally.threat)" class="icon marker threat" :style="threatIcon" :title="`Threat ${tally.threat}`" />
        </span>
      </li>
    </ol>
  </section>
</template>

<style scoped>
.tally {
  overflow: hidden;
  container-type: inline-size;
  /* tucked under the game bar, which casts its shadow over it */
  margin-top: calc(-1 * var(--gap));
  background: color-mix(in srgb, var(--forest) 45%, var(--surface-2));
  border: 1px solid var(--line);
  border-top: 0;
  border-radius: 0 0 6px 6px;
  box-shadow: 0 2px 8px var(--shadow);
  user-select: none;
  flex: 0 0 auto;
}

.track {
  display: grid;
  grid-template-columns: repeat(51, minmax(0, 1fr));
  margin: 0;
  padding: 0;
  list-style: none;
}
/* each space: willpower marker, number, threat marker */
.track li {
  display: grid;
  grid-template-columns: 1fr auto 1fr;
  align-items: center;
  padding: 0.25rem 0;
  border-left: 1px solid var(--line);
}
.track li:first-child { border-left: 0; }
.track li.ahead { background: var(--highlight-tint); }
.track li.ahead .number { color: var(--accent-bright); }

.number {
  font-size: 0.6rem;
  font-variant-numeric: tabular-nums;
  color: var(--cream-faint);
}
.tick .number { color: var(--text); font-weight: 700; }
/* the spaces the markers are on, tinted a muted version of each marker */
.track li.on-willpower { --tint-left: color-mix(in srgb, var(--accent-bright) 22%, transparent); background: var(--tint-left); }
.track li.on-threat { --tint-right: color-mix(in srgb, var(--threat-red) 22%, transparent); background: var(--tint-right); }
.track li.on-willpower.on-threat { background: linear-gradient(90deg, var(--tint-left) 50%, var(--tint-right) 50%); }
.track li.on-willpower .number, .track li.on-threat .number { color: var(--text); font-weight: 700; }

/* too narrow for a marker either side of every number: show just the totals */
.summary { display: none; margin: 0; padding: 0.25rem 0.6rem; gap: 1rem; justify-content: center; align-items: center; font-size: 0.8rem; font-weight: 700; }
.total { display: flex; align-items: center; gap: 0.3rem; }
.summary .marker { font-size: 14px; }
.last-round { font-size: 0.65rem; font-weight: 400; text-transform: uppercase; letter-spacing: 0.1em; color: var(--text-muted); }
@container (max-width: 1400px) {
  .summary { display: flex; }
  .track { display: none; }
}

.slot { display: grid; place-items: center; min-width: 0; }
.marker { font-size: 11px; filter: drop-shadow(0 1px 2px var(--shadow)); }
.marker.willpower { color: var(--accent-bright); }
.marker.threat { color: var(--threat-red); }
.marker.last { color: var(--accent-bright); opacity: 0.4; filter: none; }
</style>
