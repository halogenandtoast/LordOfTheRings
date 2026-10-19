<script setup lang="ts">
import type { TallyView } from '../types'

defineProps<{ tally: TallyView }>()

const max = 50
const steps = Array.from({ length: max + 1 }, (_, n) => n)
// totals past the end of the track sit on its last space
const at = (value: number) => Math.min(value, max)
const willpowerIcon = { '--icon': "url('/images/icons/game/willpower.svg')" }
const threatIcon = { '--icon': "url('/images/icons/game/threat.svg')" }
// markers ride over the track rather than sitting in a space, so they can slide between spaces
const on = (value: number) => ({ '--n': at(value) })
</script>

<template>
  <section class="tally" :aria-label="$t('game.tally.label')">
    <p class="summary">
      <span class="total"><span class="icon marker willpower" :style="willpowerIcon" /> {{ tally.willpower }}</span>
      <span v-if="tally.lastWillpower !== null" class="total last-round">{{ $t('game.tally.lastRound', { value: tally.lastWillpower }) }}</span>
      <span class="total"><span class="icon marker threat" :style="threatIcon" /> {{ tally.threat }}</span>
    </p>
    <div class="rail">
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
          <span class="number">{{ n }}</span>
        </li>
      </ol>
      <span
        v-if="tally.lastWillpower !== null"
        class="icon marker left last"
        :style="[willpowerIcon, on(tally.lastWillpower)]"
        :title="$t('game.tally.lastRoundTitle', { value: tally.lastWillpower })"
      />
      <span class="icon marker left willpower" :style="[willpowerIcon, on(tally.willpower)]" :title="$t('game.tally.willpower', { value: tally.willpower })" />
      <span class="icon marker right threat" :style="[threatIcon, on(tally.threat)]" :title="$t('game.threat', { value: tally.threat })" />
    </div>
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

.tally {
  --move: 0.45s ease;
  /* a space's width, and the gap either side of its number where the markers sit */
  --space: calc(100% / 51);
  --number-width: 0.75rem;
  --side: calc((var(--space) - var(--number-width)) / 2);
}
@media (prefers-reduced-motion: reduce) {
  .tally { --move: 0s; }
}
.rail { position: relative; }
.track {
  display: grid;
  grid-template-columns: repeat(51, minmax(0, 1fr));
  margin: 0;
  padding: 0;
  list-style: none;
}
/* each space: willpower marker, number, threat marker */
.track li {
  position: relative;
  isolation: isolate;
  display: grid;
  place-items: center;
  padding: 0.25rem 0;
  border-left: 1px solid var(--line);
  transition: background-color var(--move);
}
.track li:first-child { border-left: 0; }
.track li.ahead { background: var(--highlight-tint); }
.track li.ahead .number { color: var(--accent-bright); }

.number {
  width: var(--number-width);
  text-align: center;
  font-size: 0.6rem;
  font-variant-numeric: tabular-nums;
  color: var(--cream-faint);
}
.tick .number { color: var(--text); font-weight: 700; }
/* the spaces the markers are on, tinted a muted version of each marker; the tints are
   layers that fade, so the old space fades back as the new one fades in */
.track li::before, .track li::after {
  content: '';
  position: absolute;
  inset: 0;
  z-index: -1;
  opacity: 0;
  transition: opacity var(--move);
}
.track li::before { background: color-mix(in srgb, var(--accent-bright) 22%, transparent); }
.track li::after { background: color-mix(in srgb, var(--threat-red) 22%, transparent); }
.track li.on-willpower::before, .track li.on-threat::after { opacity: 1; }
.track li.on-willpower.on-threat::before { right: 50%; }
.track li.on-willpower.on-threat::after { left: 50%; }
.track li.on-willpower .number, .track li.on-threat .number { color: var(--text); font-weight: 700; }

/* too narrow for a marker either side of every number: show just the totals */
.summary { display: none; margin: 0; padding: 0.25rem 0.6rem; gap: 1rem; justify-content: center; align-items: center; font-size: 0.8rem; font-weight: 700; }
.total { display: flex; align-items: center; gap: 0.3rem; }
.summary .marker { font-size: 14px; }
.last-round { font-size: 0.65rem; font-weight: 400; text-transform: uppercase; letter-spacing: 0.1em; color: var(--text-muted); }
@container (max-width: 1400px) {
  .summary { display: flex; }
  .rail { display: none; }
}

.rail .marker {
  position: absolute;
  top: 50%;
  translate: -50% -50%;
  transition: left var(--move);
}
.rail .marker.left { left: calc(var(--n) * var(--space) + var(--side) / 2); }
.rail .marker.right { left: calc((var(--n) + 1) * var(--space) - var(--side) / 2); }
.marker { font-size: 11px; filter: drop-shadow(0 1px 2px var(--shadow)); }
.marker.willpower { color: var(--accent-bright); }
.marker.threat { color: var(--threat-red); }
.marker.last { color: var(--accent-bright); opacity: 0.4; filter: none; }
</style>
